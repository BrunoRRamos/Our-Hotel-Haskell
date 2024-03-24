module MainMenu
  ( loop,
  )
where

import CreateUser (createUser)
import Rooms (roomsLoop)
import System.Exit (die)
import Util.LoginLoop (loginLoop)

loop :: [String] -> IO ()
loop args = do
  conn <- startDb
  putStrLn "\nAvailable commands:"
  putStrLn "  login"
  putStrLn "  clients"
  putStrLn "  room"
  putStrLn "  service"
  putStrLn " test"
  putStrLn "  exit - Quit the program"
  putStrLn "\nEnter a command: "
  cmd <- getLine
  let nextArgs = words cmd
  case head nextArgs of
    "login" -> do
      loginLoop args
    "room" -> do
      roomsLoop args
    -- FOR TESTING PURPOSES
    "2" -> do
      createUser
        conn
        User
          { _firstName = "James",
            _lastName = "Bond",
            _email = "007@gmail.com",
            _password = "password",
            _isActive = True,
            _role = ADMIN
          }
      user <- getUser conn "007@gmail.com"
      print user
      print $ _role user
      loop args
    "3" -> do
      createRoom
        conn
        Room
          { Models.Room._id = 505,
            _dailyRate = 200,
            _status = "AVAILABLE",
            _occupancy = 2
          }
      room <- getRoom conn 505
      print "room created!"
      print room

      let start = parseTimeOrError True defaultTimeLocale "%F" "2021-12-01"
      let end = parseTimeOrError True defaultTimeLocale "%F" "2021-12-05"
      createReservation
        conn
        Reservation
          { _roomId = 505,
            _userId = "007@gmail.com",
            _start = start,
            _end = end,
            _blockServices = False,
            _rating = Nothing
          }
      reservation <- getReservation conn 1
      print reservation

      loop args
    "4" -> do
      createService
        conn
        Service
          { _reservationId = 1,
            _type = MEAL,
            _price = 50,
            _description = "A yummy yummy breakfast"
          }
      service <- getService conn 1
      print service
    "5" -> do
      die "Goodbye!"
    _ -> do
      putStrLn "Invalid command. Please try again."
      loop args
  loop args
