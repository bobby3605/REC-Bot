{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Text (isPrefixOf, toLower, Text, append, pack, unpack)
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import Discord.Requests as R
import System.Environment
import Xeno.DOM
import qualified Data.ByteString.Char8 as B8
import Data.Word
import qualified Data.List as L
import Data.Maybe
import Xeno.Types
import Control.Monad.IO.Class
import Discord.Internal.Rest.Prelude

type BotToken = Text

type MET = Maybe ErrorTypes

data ErrorTypes = DuplicateKeys String | NotFound String

data BotInfo = BotInfo
  { welcomeChannelId :: ChannelId
  , usersRoleId      :: RoleId
  , serverId          :: GuildId
  , rulesChannelId    :: ChannelId
  }

parseFile :: String -> ([Maybe BotInfo], Maybe BotToken)
parseFile file = (botInfosHelper,botTokenHelper)
  where mainNode = (parse (B8.pack file))
        botTokenHelper :: Maybe BotToken
        botTokenHelper = case findChild mainNode "botToken" of
          Nothing -> Nothing
          Just a -> Just (pack $ init $ drop 6 $ show $ head $ contents a)
        botInfosHelper :: [Maybe BotInfo]
        botInfosHelper = map botInfoBuilder listOfServerNodes
          where botInfoBuilder :: Maybe Node -> Maybe BotInfo
                botInfoBuilder node = case node of
                  Nothing -> Nothing
                  Just n -> Just $ BotInfo { welcomeChannelId = getSnowflake "welcomeChannelId"
                                           , usersRoleId = getSnowflake "usersRoleId"
                                           , serverId    = getSnowflake "serverId"
                                           , rulesChannelId = getSnowflake "rulesChannelId"
                                           }
                    where f = searchXMLForContents $ Right n
                          getSnowflake :: String -> Snowflake
                          getSnowflake x = Snowflake (read (f x) :: Word64)

                listOfServerNodes :: [Maybe Node]
                listOfServerNodes = L.filter isJust $ map childrenFilter $ listOfServerNodesHelper mainNode
                listOfServerNodesHelper :: Either XenoException Node -> [Maybe Node]
                listOfServerNodesHelper eNode = case eNode of
                  Left _ -> [Nothing]
                  Right node -> map Just $ children node
                childrenFilter :: Maybe Node -> Maybe Node
                childrenFilter node = case node of
                  Nothing -> Nothing
                  Just n -> if (length (children n) > 0) && ((B8.unpack $ name n) /= "botInfo") then Just n else Nothing

findChild :: Either XenoException Node -> String -> Maybe Node
findChild eNode input = case eNode of
  Left _ -> Nothing
  Right node -> case L.find (\n -> input == (B8.unpack $ name n)) (children node) of
    Nothing -> Nothing
    Just a -> Just a

searchXMLForContents :: Either XenoException Node -> String -> String
searchXMLForContents xml input = case xml of
  Left a -> "ERROR IN SEARCHING FOR CONTENTS: " ++ (show a)
  Right b -> case helper b input of
    Left e -> case e of
      Nothing -> "ERROR IN SEARCHING FOR CONTENTS 2"
      Just x -> case x of
        DuplicateKeys a -> "DUPLICATE KEYS: " ++ a
        NotFound a -> "NOT FOUND: " ++ a
    Right x -> x
  where helper :: Node -> String -> Either MET String
        helper node s = if s==(B8.unpack $ name node) then Right (init $ drop 6 $ show $ head $ contents node) else
            case checkChildren node of
              Nothing -> Left $ Just $ NotFound s
              Just n -> helperCheck $ map (\x -> helper x s) (children n)

        checkChildren :: Node -> Maybe Node
        checkChildren node = if length (children node) > 0 then Just node else Nothing
        helperCheck :: [Either MET String] -> Either MET String
        helperCheck x = case L.find isJust (map myFind x) of
          Nothing -> Left $ Just $ NotFound input
          Just n -> case n of
            Nothing -> Left $ Just $ NotFound input
            Just a -> Right a

          where myFind :: Either MET String -> Maybe String
                myFind b = case b of
                  Left _ -> Nothing
                  Right a -> Just a
main :: IO ()
main = do
  args <- getArgs
  if (length args) /= 1 then putStrLn "Invalid argument count" else do
    file <- readFile (head args)
    let (maybeBotInfos,maybeToken) = parseFile file in do
      case maybeToken of
        Nothing -> putStrLn "Failed to get botToken"
        Just botToken -> let botInfos = map helper $ L.filter isJust maybeBotInfos in do
          runDiscord (def { discordToken = botToken
                          , discordOnEvent = eventHandler botInfos }) >>= TIO.putStrLn
  where helper :: Maybe BotInfo -> BotInfo
        helper m = case m of
          Just a -> a

getGuildID :: Maybe GuildId -> GuildId
getGuildID gid = case gid of
  Nothing -> 0
  Just a -> a

catchErrors :: FromJSON a => DiscordHandler (Either RestCallErrorCode a) -> DiscordHandler a
catchErrors x = do
  t <- x
  case t of
    Left a -> do
      liftIO $ putStrLn $ show a
      mzero
    Right b -> pure b

eventHandler :: [BotInfo] -> Event -> DiscordHandler ()
eventHandler botInfos event = case event of
  -- Commands
  MessageCreate m -> when (not (fromBot m)) $ do
    case (unpack $ messageText m) of
      "!roleids" -> (restCallChecked $ R.GetGuildRoles $ getGuildID $ messageGuild m) >>=
        (\roles -> sendMessage (pack ("Role IDs: " ++ (concatMap show roles))) (messageChannel m))
      "!serverid" -> sendMessage (pack ("Server ID: " ++ (show $ getGuildID $ messageGuild m))) (messageChannel m)
      "!channelid" -> sendMessage (pack ("Channel ID: " ++ (show $ messageChannel m))) (messageChannel m)
      "!ping" -> sendMessage (pack "pong") (messageChannel m)
      "!welcomeTest" -> sendMessage (append "Welcome " (userName $ messageAuthor m)) $ welcomeChannel $ getGuildID $ messageGuild m
      _ -> do
        if "i agree" `L.isInfixOf` (unpack $ toLower $ messageText m) then
          if ((messageChannel m) == (rulesChannel guildid)) then do
            member <- restCallChecked $ R.GetGuildMember guildid (userId $ messageAuthor m)
            if not ((usersRole guildid) `elem` (memberRoles member)) then
              (restCallChecked $ AddGuildMemberRole guildid (userId $ memberUser member) $ usersRole guildid) >>
              pure ()
            else pure ()
          else pure ()
        else pure ()
    where guildid = getGuildID $ messageGuild m
  -- New User
  GuildMemberAdd gid member -> (sendMessage (append "Welcome " (userName $ memberUser member)) $ welcomeChannel gid) >> pure ()
  -- Anything else
  _ -> pure ()
  where welcomeChannel gid = welcomeChannelId $ getBotInfo botInfos gid
        usersRole      gid = usersRoleId $ getBotInfo botInfos gid
        rulesChannel   gid = rulesChannelId $ getBotInfo botInfos gid

restCallChecked :: (FromJSON a, Request (r a)) => r a -> DiscordHandler a
restCallChecked = catchErrors . restCall

getBotInfo :: [BotInfo] -> GuildId -> BotInfo
getBotInfo infos gid = head $ L.filter (\info -> gid==(serverId info)) infos

sendMessage :: Text -> ChannelId -> DiscordHandler ()
sendMessage m channelid = (restCallChecked (R.CreateMessage channelid m)) >> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)
