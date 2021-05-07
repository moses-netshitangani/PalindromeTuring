{- This is a basic turing machine that checks to see if strings are
palindromes. However, one special state was added and this is the
'Decline' state. This was done to prevent the code from looping indefinitely.
The machine prints out all the changes made to the tape, one at a time as the
turing machine is running. In the end, it either prints an 'a' or a 'b'. Only
{a,b} can be used as symbols (The rest of the symbols will raise an exception).
Strings cannot have length that is greater than 4. To type in a blank, just
hit 'Enter' when the turing function asks for a string. Strings should be typed
without quotation marks around them.

Usage Example:
1. Load the turing.hs file
2. Call the 'turing' function from the cmd
3. It'll ask you for a string. Type it in without quotation marks.
4. See how the turing machine computes the result.
-}

{- 1. States -}
data State = Start | Accept | B | C | D | E | F | G | Decline deriving Eq


{- 2. Input/Output symbols -}
data Symbol = Aa | Bb | Blank deriving Eq

-- converts character into a Symbol type
to_symbol :: Char -> Symbol
to_symbol a   | a == 'a' = Aa
              | a == 'b' = Bb
              | a == '-' = Blank

-- converts a Symbol type into a character
from_symbol :: Symbol -> Char
from_symbol a | a == Aa = 'a'
              | a == Bb = 'b'
              | a == Blank = '-'


{- 3. The transition function. Takes in state and input symbol, outputs
new state, the symbol to replace the input symbol with, and the
direction in which the tape head must move -}
transition_func :: (State, Symbol) -> (State, Symbol, Direction)

transition_func (Start, Aa) = (B, Blank, R)
transition_func (Start, Bb) = (D, Blank, R)
transition_func (Start, Blank) = (Accept, Blank, L)

transition_func (B, Aa) = (B, Aa, R)
transition_func (B, Bb) = (B, Bb, R)
transition_func (B, Blank) = (C, Blank, L)

transition_func (C, Aa) = (G, Blank, L)
transition_func (C, Bb) = (Decline, Bb, L)
transition_func (C, Blank) = (Accept, Blank, L)

transition_func (D, Aa) = (D, Aa, R)
transition_func (D, Bb) = (D, Bb, R)
transition_func (D, Blank) = (E, Blank, L)

transition_func (E, Aa) = (Decline, Aa, L)
transition_func (E, Bb) = (F, Blank, L)
transition_func (E, Blank) = (Accept, Blank, L)

transition_func (F, Aa) = (F, Aa, L)
transition_func (F, Bb) = (F, Bb, L)
transition_func (F, Blank) = (Start, Blank, R)

transition_func (G, Aa) = (G, Aa, L)
transition_func (G, Bb) = (G, Bb, L)
transition_func (G, Blank) = (Start, Blank, R)


{- 4. Converts input string into a tape and initializes the tape head -}
init_tape :: String -> [String]
init_tape str = ["--" ++ str ++ "--", tape_head (head_move 0 Initial)]


{- 5. Represents the tape head in string format -}
tape_head :: Int -> String
tape_head 0 = "^"
tape_head n = " " ++ tape_head (n - 1)


{- 6. Responsible for moving the tape head by a single unit -}
data Direction = Initial | L | R deriving Eq

head_move :: Int -> Direction -> Int
head_move _ Initial = 2 -- initial position of head is 2 because we embed 2 blanks before the input string
head_move pos L = pos - 1
head_move pos R = pos + 1

{- 7. Updates the tape by performing the 'substitution' of symbols -}
update_tape :: String -> Symbol -> Int -> String
update_tape str sym pos = take pos str ++ [from_symbol sym] ++ [str !! x | x <- [pos + 1..(length str - 1)]]

---------------------------------------------------------------------------
{- 8. The function to call on GHCi. Takes user input, creates a tape and a
tape head, and feeds the tape to the turing machine -}
turing :: IO ()
turing = do
    putStrLn "Hi. Type in a string to see if it's a palindrome:" 
    name <- getLine
    -- start machine with initialized tape
    if name /= "" then
        if length name < 5 then do
            putStrLn "----------Turing Machine Computing----------"
            turing_machine (init_tape name) Start 2
        else putStrLn "String exceeds maximum number of allowed characters (4)"
    else
        putStrLn "a"
---------------------------------------------------------------------------

{- 9. Iterates through each input character, matching each first character
and each last character (replacing them with blanks if they match). -}
turing_machine :: [String] -> State -> Int -> IO()

turing_machine (str:tape_str) state pos =
    if (state /= Accept) && (state /= Decline) 
        then do
            putStrLn str
            putStrLn $ head tape_str
            -- call the turing machine with new tape, next state, and position of next symbol
            turing_machine [(update_tape str new_symbol pos), tape_head new_position] new_state new_position
    else
        if state == Accept then do
            putStrLn "----------Turing Machine Halted----------"
            putStrLn "a"
        else do
            putStrLn "----------Turing Machine Halted----------" 
            putStrLn "b"
    where
        new_state = get_state (transition_func (state, to_symbol (str !! pos)))
        new_symbol = get_symbol (transition_func (state, to_symbol (str !! pos)))
        new_direction = get_direction (transition_func (state, to_symbol (str !! pos)))
        new_position = head_move pos new_direction


{- 10. Auxiliary functions for accessing the 3 tuple returned by the
transition function -}
get_state :: (State, Symbol, Direction) -> State
get_state (x,_,_) = x

get_symbol :: (State, Symbol, Direction) -> Symbol
get_symbol (_,x,_) = x

get_direction :: (State, Symbol, Direction) -> Direction
get_direction (_,_,x) = x