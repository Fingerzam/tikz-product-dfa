
data State = State String Int

states = ["e", "a", "aa", "aaa"]
transition = [((states !! i, "a"), states !! (i+1)) | i <- [0,1,2]] ++
             [((states !! i, "b"), states !! i) | i <- [0,1,2,3]] ++
             [(("aaa", "a"), "aaa")]


states' = ["e", "b", "bb"]
transition' = [((states' !! i, "b"), states' !! (i+1)) | i <- [0,1]] ++
             [((states' !! i, "a"), states' !! i) | i <- [0,1,2]] ++
             [(("bb", "b"), "bb")]

rotate "right" = "above"
rotate "below" = "left"
rotate _ = "above"

lineDFA states transition direction = nodes ++ "\n\n" ++ path
  where nodes = headNode (head states) ++ concatMap normalNode statePairs
        statePairs = zip (tail states) states
        headNode state = "\\node[state, initial] (q" ++ state ++ ") " ++
                         "{$q_{" ++ state ++ "}$};\n"
        normalNode (state, previous) = "\\node[state] (q" ++ state ++ ") " ++
                                       "["++direction++" of=q"++previous++"] " ++
                                       "{$q_{" ++ state ++ "}$};\n"

        path = "\\path " ++ paths ++ ";"
        paths = concatMap edge transition
        edge ((from, with), to) = "(q"++from++") edge"++loop++" node " ++
                                  "{$"++with++"$} " ++ "(q"++to++")\n"
          where loopDirection = rotate direction
                loop | from == to = " [loop "++loopDirection++"] "
                     | otherwise  = ""

leftRightDFA states transition = lineDFA states transition "right"
topDownDFA states transition = lineDFA states transition "below"

productDFA states1 states2 transition1 transition2 =
    nodes ++ "\n" ++ path
  where nodes = cornerNode cornerState ++
                concatMap firstColumnNode (zip (tail firstColumnStates)
                                               firstColumnStates) ++
                otherStates
        cornerState = (head states1, head states2)
        firstColumnStates = [(head states1, state2) | state2 <- states2]
        row state = [(state1, state) | state1 <- states1]
        rows = map row states2
        otherStates = concatMap rowStates rows
        rowStates row = concatMap node $ zip (tail row) row

        cornerNode name@(name1, name2) = declaration (name1++name2) ++
                                    tag name ++ ";\n"
        declaration name = "\\node[state] " ++ "(q"++name++")"
        tag (name1, name2) = "{$(q_{"++name1++"}, q_{"++name2++"})$}"

        firstColumnNode (name@(name1,name2), (above1, above2)) =
            declaration (name1++name2) ++
            belowDeclaration (above1++above2) ++
            tag name ++ ";\n"

        belowDeclaration above = "[below of=q"++above++"]"

        node (name@(name1,name2), (left1,left2)) =
            declaration (name1++name2) ++
            rightDeclaration (left1++left2) ++
            tag name ++ ";\n"
        rightDeclaration left = "[right of=q"++left++"]"

        productTransition = [(((from1, from2), with), (to1, to2)) |
                 ((from1, with), to1) <- transition1,
                 ((from2, with2), to2) <- transition2,
                 with == with2]

        stateName (name1,name2) = name1 ++ name2
        edge ((from, with), to) = "(q"++(stateName from)++") edge"++
                                  loop++" node " ++ "{$"++with++"$} " ++
                                  "(q"++(stateName to)++")\n"
          where loopDirection = "below"
                loop | from == to = " [loop "++loopDirection++"] "
                     | otherwise  = ""

        edges = concatMap edge productTransition
        path = "\\path \n" ++ edges ++ ";\n"


productTransition transition1 transition2 =
    [(((from1, from2), with), (to1, to2)) |
     ((from1, with), to1) <- transition1,
     ((from2, with2), to2) <- transition2,
     with == with2]
