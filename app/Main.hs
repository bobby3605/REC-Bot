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

type Token = Text

type MET = Maybe ErrorTypes

data BotInfo = BotInfo
  { welcomeChannelId :: ChannelId
  , usersRoleId      :: RoleId
  , botToken      :: Text
  , debug            :: Bool
  }

data ErrorTypes = DuplicateKeys String | NotFound String

parseFile :: String -> BotInfo
parseFile file = BotInfo { welcomeChannelId = Snowflake (read (searchXMLForContents file "welcomeChannelId") :: Word64)
                         , usersRoleId = Snowflake (read (searchXMLForContents file "usersRoleId") :: Word64)
                         , botToken = pack $ searchXMLForContents file "botToken"
                         , debug       = if (searchXMLForContents file "debug") == "True" then True else False
                         }

searchXMLForContents :: String -> String -> String
searchXMLForContents file input = case parse (B8.pack file) of
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
                myFind x = case x of
                  Left a -> Nothing
                  Right a -> Just a

main :: IO ()
main = do
  args <- getArgs
  if (length args) /= 1 then putStrLn "Invalid argument count" else do
    file <- readFile (head args)
    let botInfo = parseFile file in do
      runDiscord (def { discordToken = botToken botInfo
                      , discordOnEvent = if debug botInfo then debugHandler botInfo else eventHandler botInfo }) >>= TIO.putStrLn

debugHandler :: BotInfo -> Event -> DiscordHandler ()
debugHandler botInfo event = case event of
  MessageCreate m -> when (not (fromBot m)) $ do
    case (unpack (messageText m)) of
      "!roles" -> do
        roles <- restCall $ R.GetGuildRoles $ getGuildID $ messageGuild m
        sendMessage (pack ("Role IDs: " ++ (roleListErrorHandler roles))) (messageChannel m)
      otherwise -> sendMessage (pack ("Channel ID: " ++ (show $ messageChannel m))) (messageChannel m)
  _ -> pure ()

roleListErrorHandler :: Either RestCallErrorCode [Role] -> String
roleListErrorHandler e = case e of
  Left a -> "Error getting role: " ++ (show a)
  Right b -> concatMap show b

getGuildID :: Maybe GuildId -> GuildId
getGuildID gid = case gid of
  Nothing -> 0
  Just a -> a

eventHandler :: BotInfo -> Event -> DiscordHandler ()
eventHandler botInfo event = case event of
  -- Ping
  MessageCreate m -> when (not (fromBot m) && isPing (messageText m)) $ do
    sendMessage "pong" (messageChannel m)
  -- New User
  GuildMemberAdd gid member -> do
    sendMessage (append "Welcome " (userName $ memberUser member)) welcomeChannel
    _ <- restCall $ AddGuildMemberRole gid (userId $ memberUser member) usersRole
    pure ()
  -- Anything else
  _ -> pure ()
  where welcomeChannel = welcomeChannelId botInfo
        usersRole      = usersRoleId botInfo

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
