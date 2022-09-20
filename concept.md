# AYWinS <3

## Discord Commands

**User**
- `/iwon <game>`: Increment your score in <game>
- `/setscore <game> <n>`: Set your score in <game>
- `/amiwinning <game?>`: Get your score for all games or a specific one
- `/aretheywinning <user> <game?>`: Get another user's score for all games or a specific one
- `/whoiswinning <game?>`: Get global leaderboards for all games or a specific one

**Admin** (using an *AywinsAdmin* role)
- `/setscore <game> <n> <user?>`: Same as the user version, but can target any user instead of just self
- `/rmgame <game>`: Remove game listing (and associated scores)
- `/rmuser <user>`: Remove user from score listings
- `/banuser <user>`: Ban user from score listings
- `/unbanuser <user>`: Unban user from score listings
- `/joingames <game> <game>`: Consolidate two games (e.g. with similar/misspelt names) into one
- `/renamegame <game> <newname>`: Change name of a game

## Database Model

**Game**
- Name :: String [PK]
- Total Wins :: Int
- Latest Winner :: discord_id
- Last Won Date :: Date
- Wins :: 1-M:Win

**User**
- Discord ID :: discord_id [PK]
- Total Wins :: Int
- Wins :: 1-M:Win

**Win**
- ID :: CompK:User+Game+Date
- User :: FK:User
- Game :: FK:Game
- Date :: Date
