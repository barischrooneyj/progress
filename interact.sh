#!/usr/bin/env expect

cd backend
spawn stack ghci backend:exe:backend-exe

expect "*Main>"
send ":l src/Tutorial.hs\n"
send "db <- newStore \[\]\n"
send "run db example\n"
send "prettyLn db\n"

interact
