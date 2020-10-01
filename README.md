# discordbot
## Building:
``` stack build```
## Run:
The bot needs the app/botinfotemplate.xml file to work properly
``` stack run pathToBotInfo.xml```
### BotInfo.xml:
Send ```!roleids``` in any channel to have the bot tell all the role IDs. Set ```usersRoleId``` to the value for default user roles. <br>
Send ```!serverid``` in any channel to have the bot tell the server ID. Set ```serverId``` to this value. <br>
Send ```!channelid``` in any channel to have the bot tell the current channel ID. Set ```welcomeChannelId``` to the value for the welcome channel. <br>
Send ```!welcomeTest``` in any channel to have the bot welcome you. This is useful to make sure you set the ```welcomeChannelId``` and ```serverId``` correctly. <br>
Send ```!ping``` in any channel to have the bot respond with pong. <br>
