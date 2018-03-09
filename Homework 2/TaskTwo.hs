module TaskTwo where

data Player = Player 
    { playerAttack  :: Int
    , playerDefense :: Int
    , playerHealth  :: Int
    } deriving Show

data Monster = Monster
    { monsterAttack     :: Int
    , monsterDefense    :: Int
    , monsterHealth     :: Int
    , droppedEquipments :: [Equipment]
    } deriving Show

data Equipment = Weapon { attack :: Int }
               | Armor { defense :: Int }
               | Medicine { health :: Int }
               deriving Show

data Result = Win Player
            | Lose
            deriving Show

gloriousBattle :: Player -> [Monster] -> Result
gloriousBattle player []       = Win player
gloriousBattle player (x : xs) = case (playerToMonster player x) of
    Win p -> gloriousBattle p xs
    Lose  -> Lose

playerToMonster :: Player -> Monster -> Result
playerToMonster player monster
    | attackPower >= monsterHealth monster = Win $ improveSkills player $ droppedEquipments monster
    | otherwise                            = monsterToPlayer (monster { monsterHealth = monsterHealth monster - attackPower }) player
  where
    attackPower = max 1 (playerAttack player - monsterDefense monster)

monsterToPlayer :: Monster -> Player -> Result
monsterToPlayer monster player
    | attackPower >= playerHealth player = Lose
    | otherwise                          = playerToMonster (player { playerHealth = playerHealth player - attackPower }) monster
  where
    attackPower = max 1 (monsterAttack monster - playerDefense player)

improveSkills :: Player -> [Equipment] -> Player
improveSkills player []       = player
improveSkills player (x : xs) = improveSkill player x

improveSkill :: Player -> Equipment -> Player
improveSkill player (Weapon weapon)     = player { playerAttack = min 100 (playerAttack player + weapon) }
improveSkill player (Armor armor)       = player { playerDefense = min 100 (playerDefense player + armor) }
improveSkill player (Medicine medicine) = player { playerHealth = max 0 $ min 100 (playerHealth player + medicine) }

bestPlayer = Player { playerAttack = 80, playerDefense = 100, playerHealth = 100 }
loser      = Player { playerAttack = 100, playerDefense = 30, playerHealth = 10 }

monster1 = Monster { monsterAttack = 20, monsterDefense = 20, monsterHealth = 60, droppedEquipments = [Weapon 20] }
monster2 = Monster { monsterAttack = 100, monsterDefense = 20, monsterHealth = 80, droppedEquipments = [] }
monster3 = Monster { monsterAttack = 100, monsterDefense = 100, monsterHealth = 100, droppedEquipments = [] }
monsters = [monster1, monster2, monster3]

testWin :: Result
testWin = gloriousBattle bestPlayer monsters

testLose :: Result
testLose = gloriousBattle loser monsters
