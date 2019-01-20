module ListExtra exposing (..)

import Maybe

sequence : List (Maybe a) -> Maybe (List a)
sequence = List.foldr (Maybe.map2 (::)) (Just [])


mainDiagonal : List (List a) -> Maybe (List a)
mainDiagonal lol_ =
    case lol_ of
        [] ->
            Just []

        (xs :: xss) ->
            let
                first = List.head xs
                tails = List.map List.tail xss |> sequence
                diagonalFromTails = tails |> Maybe.andThen mainDiagonal
            in
                Maybe.map2 (::) first diagonalFromTails


transpose : List (List a) -> Maybe (List (List a))
transpose lol_ =
    case lol_ of
        ((x::xs) :: xss) ->
            let
                heads = List.map List.head lol_ |> sequence
                tails = List.map List.tail xss |> sequence
                transposedTails = tails |> Maybe.andThen transpose
                rest = Maybe.map2 (List.map2 (::)) (Just xs) transposedTails
            in
                Maybe.map2 (::) heads rest

        _ ->
            if lol_ == List.filter List.isEmpty lol_ then
                Just []
            else
                Nothing


secondaryDiagonal : List (List a) -> Maybe (List a)
secondaryDiagonal = mainDiagonal << List.map List.reverse


allTrue : List Bool -> Bool
allTrue = List.foldr (&&) True


anyTrue : List Bool -> Bool
anyTrue = List.foldr (||) False
