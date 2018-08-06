#!/usr/bin/env expect

spawn stack ghci progress:exe:progress-exe

expect "*Main>"
send ":l src/Tutorial.hs\n"
send "db <- newStore\n"
send "run db example\n"
send "prettyLn db\n"

interact
