# discordbot
## Building:
``` stack build```
## Run:
The bot needs the app/botinfotemplate.xml file to work properly
``` stack run pathToBotInfo.xml```
### BotInfo.xml:
To get the values to fill out the template, set debug to ```True``` (case-sensitive). If debug is set to anything else, it defaults to false. <br>
Send ```!roles``` in any channel to have the bot tell all the role IDs. Set ```usersRoleId``` to the value for default user roles. <br>
The bot will output the channel ID for any channel which has a non-command message sent. <br>
Set welcomeChannelId to the channel ID that you want the bot to greet users in.
