import Data.Char
knightJump::[(Int,Int)]
knightJump = [(1,2),(2,1),(2,-1),(1,-2),(-1,-2),(-2,-1),(-2,1),(-1,2)]
fullmap:: [(Char,Int)]
fullmap= [(b,a) | a <- [1..8], b<- ['a'..'h']]
kingJump::[(Int,Int)]
kingJump= [(0,1), (0,-1),(1,1),(1,0),(1,-1),(-1,1),(-1,0),(-1,-1)]
type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location| B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])

setBoard:: Board
setBoard =(White, [R ('h',1),N ('g',1),B ('f',1),K ('e',1),Q ('d',1),B ('c',1),N ('b',1),R ('a',1),P ('h',2),P ('g',2),P ('f',2),P ('e',2),P ('d',2),P ('c',2),P ('b',2),P ('a',2)] ,[R ('h',8),N ('g',8),B ('f',8),K ('e',8),Q ('d',8),B ('c',8),N ('b',8),R ('a',8),P ('h',7),P ('g',7),P ('f',7),P ('e',7),P ('d',7),P ('c',7),P ('b',7),P ('a',7)])
getSymbol:: Piece-> String
getSymbol (P (c,i)) = "P"
getSymbol (Q (c,i)) = "Q"
getSymbol (N (c,i)) = "N"
getSymbol (K (c,i)) = "K"
getSymbol (R (c,i)) = "R"
getSymbol (B (c,i)) = "B"


checklocin :: Location-> [Piece]-> Bool 
checklocin loc  []= False 
checklocin loc (h:t) = if getLocation h==loc then True else checklocin loc t
getSymbol2 :: Location-> [Piece]-> String
getSymbol2 loc (h:t)= if getLocation h==loc then getSymbol h else getSymbol2 loc t 
getpiece :: Board -> Location -> String
getpiece (_, whitePieces, blackPieces) loc   | checklocin loc whitePieces = "| " ++  getSymbol2 loc whitePieces++"W" 
                                            | checklocin loc blackPieces = "| " ++getSymbol2 loc blackPieces ++"B"
                                            |otherwise= "| "++ "  " 
 
visualizeBoard:: Board->String
visualizeBoard (turn, whitePieces, blackPieces) =
  "   a    b    c    d    e    f    g    h\n"++ unlines [show (row) ++ unwords [ getpiece (turn, whitePieces, blackPieces) (col, row)   | col <- ['a'..'h']] ++" |" | row <- [8,7..1] ]++ "\nTurn :"++ show turn 





getLocation :: Piece -> Location
getLocation (P (c,i)) = (c,i)
getLocation (Q (c,i)) = (c,i)
getLocation (N (c,i)) = (c,i)
getLocation (K (c,i)) = (c,i)
getLocation (R (c,i)) = (c,i)
getLocation (B (c,i)) = (c,i) 



inRange:: Location-> Bool
inRange (c,i) =if  (elem c ['a'..'h'] )&& (elem i [1..8] ) then True else False

isOccupied :: [Piece]->Location-> Bool
isOccupied [] _= False
isOccupied (x:xs) (c,i)  = if c==a &&i==b  then True else isOccupied  xs (c,i) where (a,b)=getLocation x 
rightDiagonal:: Location-> [Location]
rightDiagonal (c,i)=[(chr((ord c)+j), i + j) | j <- [0..8], ord c + j >= 97, ord c + j<=104, i+j >= 1, i+j <= 8, j /= 0]
leftDiagonal:: Location-> [Location]
leftDiagonal (c,i)=[(chr((ord c)-j), i + j) | j <- [0..8], ord c - j >= 97, ord c -j<=104, i+j >= 1, i+j <= 8,abs j /= 0] 

isLegal  (P (c,i)) ( _ ,whitePieces,blackPieces ) (newC,newI) | i==2 && newI==i+2 && not(isOccupied whitePieces (c, i+1))&&not(isOccupied blackPieces (c, i+1)) && c==newC &&(elem (P (c,i)) whitePieces)&& not(isOccupied whitePieces (newC,newI))&& not(isOccupied blackPieces (newC,newI))&&( inRange (newC ,newI)) = True
                                                              | newI-i==1&&c==newC&&(elem (P (c,i)) whitePieces)&&not(isOccupied whitePieces(newC,newI))&& not(isOccupied blackPieces (newC,newI)) && inRange (newC ,newI)=True
                                                              | newI-i==1&&(succ c ==newC  || succ newC==c) && (elem (P (c,i)) whitePieces) && (isOccupied blackPieces (newC,newI))&& (inRange (newC ,newI))=True
                                                              | i==7 && i==newI+2&&not(isOccupied blackPieces (c,i-1))&&not(isOccupied whitePieces (c,i-1))&&c==newC&&(elem (P (c,i)) blackPieces)&& not(isOccupied whitePieces (newC,newI))&& not(isOccupied blackPieces (newC,newI))&&( inRange (newC ,newI)) = True
                                                              | i-newI==1&&c==newC&&(elem (P (c,i)) blackPieces)&&not(isOccupied whitePieces (newC,newI))&& not(isOccupied blackPieces (newC,newI)) && inRange (newC ,newI)=True
                                                              | i-newI==1&&(succ c ==newC||succ newC==c) && (elem (P (c,i)) blackPieces) && (isOccupied whitePieces (newC,newI))&& (inRange (newC ,newI))=True
                                                              | otherwise =False
isLegal (N (c,i)) ( _ ,whitePieces,blackPieces) (newC,newI)|(elem (ord newC-ord c,(newI-i)) knightJump)&&(elem (N (c,i)) whitePieces) && not(isOccupied whitePieces (newC,newI))&& (inRange (newC ,newI))=True
                                                             |(elem (ord newC-ord c,(newI-i)) knightJump)&&(elem (N (c,i)) blackPieces) && not(isOccupied blackPieces(newC,newI))&&(inRange (newC ,newI))=True
                                                             |otherwise =False
isLegal (K (c,i)) ( _ ,whitePieces,blackPieces) (newC,newI) |(elem (ord newC-ord c,(newI-i)) kingJump) &&(elem (K (c,i)) whitePieces) && not(isOccupied whitePieces(newC,newI))&& (inRange (newC ,newI))=True
                                                            |(elem (ord newC-ord c,(newI-i)) kingJump) &&(elem (K (c,i)) blackPieces) && not(isOccupied blackPieces (newC,newI))&&(inRange (newC ,newI))=True
                                                            |otherwise= False

isLegal (B (c,i)) ( _ ,whitePieces,blackPieces) (newC,newI) | newC > c && newI > i && abs(ord newC- ord c )== abs(newI-i)   && not(foldr (||) False (map (isOccupied whitePieces) [(x,y)|(x,y)<-(rightDiagonal (c,i)),x<newC ,y<newI])) &&
                                                             not(foldr (||) False (map (isOccupied blackPieces) [(x,y)|(x,y)<-(rightDiagonal (c,i)),x<newC ,y<newI]))&&
                                                             (elem (B (c,i)) whitePieces &&not(isOccupied whitePieces (newC,newI))||(elem (B (c,i)) blackPieces )&&not(isOccupied blackPieces (newC,newI)))=True                                                    
                                                            | newC > c && newI < i && abs(ord newC- ord c) == abs(newI-i)  &&  not(foldr (||) False (map (isOccupied whitePieces) [(x,y)|(x,y)<-leftDiagonal (newC,newI),x>c ,y<i]))&&not(foldr (||) False (map (isOccupied blackPieces) [(x,y)|(x,y)<-leftDiagonal (newC,newI),x>c ,y<i]))&&(elem (B (c,i)) whitePieces &&not(isOccupied whitePieces (newC,newI))||(elem (B (c,i)) blackPieces)&&not(isOccupied blackPieces (newC,newI)))=True
                                                            | newC < c && newI > i && abs(ord newC- ord c) == abs(newI-i)   && not(foldr (||) False (map (isOccupied whitePieces) [(x,y)|(x,y)<- leftDiagonal (c,i), x>newC ,y<newI])) &&
                                                             not(foldr (||) False (map (isOccupied blackPieces) [(x,y)|(x,y)<- leftDiagonal (c,i),x>newC ,y<newI]))&&
                                                             (elem (B (c,i)) whitePieces &&not(isOccupied whitePieces (newC,newI))||(elem (B (c,i)) blackPieces )&&not(isOccupied blackPieces (newC,newI)))=True
                                                            | newC < c && newI < i && abs(ord newC- ord c )== abs(newI-i)   && not(foldr (||) False (map (isOccupied whitePieces) [(x,y)|(x,y)<- rightDiagonal (newC,newI), x<c ,y <i])) &&
                                                             not(foldr (||) False (map (isOccupied blackPieces) [(x,y)|(x,y)<- rightDiagonal (newC,newI),x<c ,y <i]))&&
                                                             (elem (B (c,i)) whitePieces&&not(isOccupied whitePieces (newC,newI))||(elem (B (c,i)) blackPieces )&&not(isOccupied blackPieces (newC,newI)))=True
                                                            |otherwise=False
                                                            
                                                          
isLegal (R(c,i)) ( _ ,whitePieces,blackPieces) (newC,newI) |newC==c && newI > i && inRange(newC,newI) &&(inpath (c,(i+1)) (newC,newI) (whitePieces,blackPieces) 'u')&& (elem (R (c,i)) whitePieces&&not(isOccupied whitePieces (newC,newI))||(elem (R (c,i)) blackPieces )&&not(isOccupied blackPieces (newC,newI))) =True
                                                           |newC==c &&newI < i &&inRange(newC,newI) && (inpath (c,(i-1)) (newC,newI) (whitePieces,blackPieces) 'd')&& (elem (R (c,i)) whitePieces&&not(isOccupied whitePieces (newC,newI))||(elem (R (c,i)) blackPieces )&&not(isOccupied blackPieces (newC,newI))) =True  
                                                           |newI ==i && newC > c &&inRange(newC,newI) &&(inpath ((chr(ord c +1)),i) (newC,newI) (whitePieces,blackPieces) 'r')&& (elem (R (c,i)) whitePieces&&not(isOccupied whitePieces (newC,newI))||(elem (R (c,i)) blackPieces )&&not(isOccupied blackPieces (newC,newI))) =True
                                                           |newI ==i && newC < c && inRange(newC,newI) &&(inpath ((chr(ord c -1)),i) (newC,newI) (whitePieces,blackPieces) 'l')&& (elem (R (c,i)) whitePieces&&not(isOccupied whitePieces (newC,newI))||(elem (R (c,i)) blackPieces )&&not(isOccupied blackPieces (newC,newI))) =True                                                          
                                                           |otherwise= False                  



isLegal (Q (c,i)) (_, whitePieces, blackPieces) (newC,newI) | newC > c && newI > i && abs(ord newC- ord c )== abs(newI-i)   && not(foldr (||) False (map (isOccupied whitePieces) [(x,y)|(x,y)<-(rightDiagonal (c,i)),x<newC ,y<newI])) &&
                                                             not(foldr (||) False (map (isOccupied blackPieces) [(x,y)|(x,y)<-(rightDiagonal (c,i)),x<newC ,y<newI]))&&
                                                             (elem (Q (c,i)) whitePieces &&not(isOccupied whitePieces (newC,newI))||(elem (Q (c,i)) blackPieces )&&not(isOccupied blackPieces (newC,newI)))=True                                                    
                                                            | newC > c && newI < i && abs(ord newC- ord c) == abs(newI-i)  &&  not(foldr (||) False (map (isOccupied whitePieces) [(x,y)|(x,y)<-leftDiagonal (newC,newI),x>c ,y<i]))&&not(foldr (||) False (map (isOccupied blackPieces) [(x,y)|(x,y)<-leftDiagonal (newC,newI),x>c ,y<i]))&&(elem (Q (c,i)) whitePieces &&not(isOccupied whitePieces (newC,newI))||(elem (Q (c,i)) blackPieces)&&not(isOccupied blackPieces (newC,newI)))=True
                                                            | newC < c && newI > i && abs(ord newC- ord c) == abs(newI-i)   && not(foldr (||) False (map (isOccupied whitePieces) [(x,y)|(x,y)<- leftDiagonal (c,i), x>newC ,y<newI])) &&
                                                             not(foldr (||) False (map (isOccupied blackPieces) [(x,y)|(x,y)<- leftDiagonal (c,i),x>newC ,y<newI]))&&
                                                             (elem (Q (c,i)) whitePieces &&not(isOccupied whitePieces (newC,newI))||(elem (Q (c,i)) blackPieces )&&not(isOccupied blackPieces (newC,newI)))=True
                                                            | newC < c && newI < i && abs(ord newC- ord c )== abs(newI-i)   && not(foldr (||) False (map (isOccupied whitePieces) [(x,y)|(x,y)<- rightDiagonal (newC,newI), x<c ,y <i])) &&
                                                             not(foldr (||) False (map (isOccupied blackPieces) [(x,y)|(x,y)<- rightDiagonal (newC,newI),x<c ,y <i]))&&
                                                             (elem (Q (c,i)) whitePieces&&not(isOccupied whitePieces (newC,newI))||(elem (Q (c,i)) blackPieces )&&not(isOccupied blackPieces (newC,newI)))=True
                                                            |newC==c && newI > i && inRange(newC,newI) &&(inpath (c,(i+1)) (newC,newI) (whitePieces,blackPieces) 'u')&& (elem (Q (c,i)) whitePieces&&not(isOccupied whitePieces (newC,newI))||(elem (Q (c,i)) blackPieces )&&not(isOccupied blackPieces (newC,newI))) =True
                                                           |newC==c &&newI < i &&inRange(newC,newI) && (inpath (c,(i-1)) (newC,newI) (whitePieces,blackPieces) 'd')&& (elem (Q (c,i)) whitePieces&&not(isOccupied whitePieces (newC,newI))||(elem (Q (c,i)) blackPieces )&&not(isOccupied blackPieces (newC,newI))) =True  
                                                           |newI ==i && newC > c &&inRange(newC,newI) &&(inpath ((chr(ord c +1)),i) (newC,newI) (whitePieces,blackPieces) 'r')&& (elem (Q (c,i)) whitePieces&&not(isOccupied whitePieces (newC,newI))||(elem (Q (c,i)) blackPieces )&&not(isOccupied blackPieces (newC,newI))) =True
                                                           |newI ==i && newC < c && inRange(newC,newI) &&(inpath ((chr(ord c -1)),i) (newC,newI) (whitePieces,blackPieces) 'l')&& (elem (Q (c,i)) whitePieces&&not(isOccupied whitePieces (newC,newI))||(elem (Q (c,i)) blackPieces )&&not(isOccupied blackPieces (newC,newI))) =True                                                          
                                                           |otherwise= False  
                                                        
inpath :: Location ->Location ->([Piece],[Piece])-> Char -> Bool 
inpath (c,i) (newC,newI) (whitePieces,blackPieces)  direc | i== newI && c==newC =True 
                                                          |direc =='u' &&not(isOccupied whitePieces (c,i))&& not(isOccupied blackPieces (c,i)) = inpath (c,(i+1)) (newC,newI) (whitePieces , blackPieces) direc 
                                                          |direc=='d' &&not(isOccupied whitePieces (c,i))&& not(isOccupied blackPieces (c,i)) = inpath (c,(i-1)) (newC,newI) (whitePieces , blackPieces) direc 
                                                          |direc=='r' &&not(isOccupied whitePieces (c,i))&& not(isOccupied blackPieces (c,i)) = inpath ((chr (ord c +1)),i) (newC,newI) (whitePieces , blackPieces) direc 
                                                          |direc=='l' &&not(isOccupied whitePieces (c,i))&& not(isOccupied blackPieces (c,i)) = inpath ((chr(ord c -1)),i) (newC,newI) (whitePieces , blackPieces) direc 
                                                          |otherwise= False

suggestMove :: Piece ->Board->[Location]
suggestMove piece board = checksuggest piece board fullmap
checksuggest :: Piece ->Board -> [Location]->[Location]
checksuggest piece board [] = []
checksuggest piece board (h:t)  = if isLegal piece board h then h:checksuggest piece board t else checksuggest piece board t 
hisTurn :: Piece-> Board->Bool
hisTurn piece (player,whitePieces,blackPieces) | elem piece whitePieces && player==White = True 
                                               | elem piece blackPieces && player== Black = True 
                                               | otherwise=False

removeloc :: Location-> [Piece]-> [Piece]
removeloc _ []=[]
removeloc loc (h:t) = if (getLocation h)==loc then t else h:removeloc loc t 
removepiece ::Piece-> [Piece]-> [Piece]
removepiece piece [] = error "piece isnt there"
removepiece piece (h:t) = if piece==h then t else h:removepiece piece t 
updateloc :: Piece -> Location -> Piece
updateloc piece loc | getSymbol piece=="P" = P loc
                    | getSymbol piece=="Q" = Q loc 
                    | getSymbol piece=="K" = K loc 
                    | getSymbol piece=="N" = N loc 
                    | getSymbol piece =="R" = R loc 
                    | getSymbol piece=="B" = B loc 


move:: Piece -> Location -> Board -> Board
move piece loc (player, whitePieces, blackPieces) | not (hisTurn piece (player,whitePieces,blackPieces)) && player==Black = error "This is Black player's turn, White can't move."
                                                  |not (hisTurn piece (player,whitePieces,blackPieces))  && player==White = error "This is White player's turn, Black can't move."
                                                  |not (isLegal piece (player,whitePieces,blackPieces) loc )= error (" Illegal move for piece: " ++ show piece)
                                                  |isLegal piece (player,whitePieces,blackPieces) loc && elem piece whitePieces && not(isOccupied blackPieces loc)= (Black,  (removepiece piece whitePieces)++[updateloc piece loc],blackPieces)
                                                  |isLegal piece (player,whitePieces,blackPieces)loc  && elem piece blackPieces && not( isOccupied whitePieces loc)=(White,whitePieces,(removepiece piece blackPieces)++[updateloc piece loc])
                                                  |isLegal piece (player,whitePieces,blackPieces) loc && elem piece whitePieces && isOccupied blackPieces loc= (Black,(removepiece piece whitePieces)++[updateloc piece loc], removeloc loc blackPieces)
                                                  |isLegal piece (player,whitePieces,blackPieces)loc  && elem piece blackPieces && isOccupied whitePieces loc=(White,removeloc loc whitePieces,(removepiece piece blackPieces)++[updateloc piece loc])
                                                  