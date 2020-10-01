{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when)
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

type BotToken = Text

type MET = Maybe ErrorTypes

data BotInfo = BotInfo
  { welcomeChannelId :: ChannelId
  , usersRoleId      :: RoleId
  , serverId          :: GuildId
  }

data ErrorTypes = DuplicateKeys String | NotFound String

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
                  Just n -> Just $ BotInfo { welcomeChannelId = Snowflake (read (f "welcomeChannelId") :: Word64)
                                           , usersRoleId = Snowflake (read (f "usersRoleId") :: Word64)
                                           , serverId    = Snowflake (read (f "serverId") :: Word64)
                                           }
                    where f = searchXMLForContents $ Right n

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
{-
findParent :: Node -> String -> Maybe Node
findParent node input = case (B8.unpack $ name node) of
  input -> Just node
  _ -> if (length $ children node) == 0 then Nothing else case L.find isJust (map (\x -> findParent x input) (children node)) of
    Nothing -> Nothing
    -- L.find returns Maybe, so a is Just a, the expanded type would be Just $ Just a
    Just a -> a
-}

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

roleListErrorHandler :: Either RestCallErrorCode [Role] -> String
roleListErrorHandler e = case e of
  Left a -> "Error getting role: " ++ (show a)
  Right b -> concatMap show b

getGuildID :: Maybe GuildId -> GuildId
getGuildID gid = case gid of
  Nothing -> 0
  Just a -> a

eventHandler :: [BotInfo] -> Event -> DiscordHandler ()
eventHandler botInfos event = case event of
  -- Commands
  MessageCreate m -> when (not (fromBot m)) $ do
    case (unpack (messageText m)) of
      "!roleids" -> do
        roles <- restCall $ R.GetGuildRoles $ getGuildID $ messageGuild m
        sendMessage (pack ("Role IDs: " ++ (roleListErrorHandler roles))) (messageChannel m)
      "!serverid" -> do
        sendMessage (pack ("Server ID: " ++ (show $ getGuildID $ messageGuild m))) (messageChannel m)
      "!channelid" -> do
        sendMessage (pack ("Channel ID: " ++ (show $ messageChannel m))) (messageChannel m)
      "ping" -> do
        sendMessage (pack "pong") (messageChannel m)
      "!welcomeTest" -> do
        sendMessage (append "Welcome " (userName $ messageAuthor m)) $ welcomeChannel $ getGuildID $ messageGuild m
      _ -> pure ()
  -- New User
  GuildMemberAdd gid member -> do
    sendMessage (append "Welcome " (userName $ memberUser member)) $ welcomeChannel gid
    _ <- restCall $ AddGuildMemberRole gid (userId $ memberUser member) $ usersRole gid
    pure ()
  -- Anything else
  _ -> pure ()
  where welcomeChannel gid = welcomeChannelId $ getBotInfo botInfos gid
        usersRole      gid = usersRoleId $ getBotInfo botInfos gid

getBotInfo :: [BotInfo] -> GuildId -> BotInfo
getBotInfo infos gid = head $ L.filter (\info -> gid==(serverId info)) infos

sendMessage :: Text -> ChannelId -> DiscordHandler ()
sendMessage m channelid = do
  _ <- restCall (R.CreateMessage channelid m)
  pure ()

memberToNick :: GuildMember -> Text
memberToNick member = case (memberNick member) of
  Nothing -> "User does not have a name"
  Just _ -> pack $ show member

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: Text -> Bool
isPing = ("ping" `isPrefixOf`) . toLower
