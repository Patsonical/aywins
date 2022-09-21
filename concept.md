# AYWinS <3

## Discord Commands

**User**
- `/iwon <game>`: Increment your score in <game>
- `/setscore <n> <game>`: Set your score in <game>
- `/amiwinning <game?>`: Get your score for all games or a specific one
- `/aretheywinning <user> <game?>`: Get another user's score for all games or a specific one
- `/whoiswinning <game?>`: Get global leaderboards for all games or a specific one
- `/addself`: Manually add self
- `/rmself`: Remove self from AYWinS
- `/aywins-help`: Display help

**Admin** (using an *AywinsAdmin* role)
- `/theywon <user> <game>`: Increment <user>'s score in <game>
- `/setscore <user?> <n> <game>`: Same as the user version, but can target any user instead of just self
- `/addgame <game>`: Manually add game listing
- `/rmgame <game>`: Remove game listing (and associated scores)
- `/adduser <user>`: Manually add user
- `/rmuser <user>`: Remove user from AYWinS
- `/banuser <user>`: Ban user from score listings
- `/unbanuser <user>`: Unban user from score listings
- `/joingames <game>; <game> [; ...]`: Consolidate two games (e.g. with similar/misspelt names) into one (literal `;` is `;;`)
- `/renamegame <game>; <newname>`: Change name of a game (literal `;` is `;;`)

## Database Model

**Game**
- Name :: String

**User**
- Discord ID :: ~~DiscordId~~ ByteString
- Banned :: Bool

**Wins**
- User :: FK:User
- Game :: FK:Game
- Score :: Int
- LastWin :: UTCTime
