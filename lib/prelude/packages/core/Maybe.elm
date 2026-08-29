{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/core -- https://github.com/elm/core
   Copyright 2014-present Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Maybe exposing
    ( Maybe(..), andThen, map, map2, map3, map4, map5, withDefault
    )


type Maybe a
    = Just a
    | Nothing


withDefault : a -> Maybe a -> a
withDefault default maybe =
    case maybe of
        Just value ->
            value

        Nothing ->
            default


map : (a -> b) -> Maybe a -> Maybe b
map f maybe =
    case maybe of
        Just value ->
            Just (f value)

        Nothing ->
            Nothing


andThen : (a -> Maybe b) -> Maybe a -> Maybe b
andThen callback maybeValue =
    case maybeValue of
        Just value ->
            callback value

        Nothing ->
            Nothing


map2 : (a -> b -> value) -> Maybe a -> Maybe b -> Maybe value
map2 func ma mb =
  case ma of
    Nothing ->
      Nothing

    Just a ->
      case mb of
        Nothing ->
          Nothing

        Just b ->
          Just (func a b)



map3 : (a -> b -> c -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe value
map3 func ma mb mc =
  case ma of
    Nothing ->
      Nothing

    Just a ->
      case mb of
        Nothing ->
          Nothing

        Just b ->
          case mc of
            Nothing ->
              Nothing

            Just c ->
              Just (func a b c)



map4 : (a -> b -> c -> d -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe value
map4 func ma mb mc md =
  case ma of
    Nothing ->
      Nothing

    Just a ->
      case mb of
        Nothing ->
          Nothing

        Just b ->
          case mc of
            Nothing ->
              Nothing

            Just c ->
              case md of
                Nothing ->
                  Nothing

                Just d ->
                  Just (func a b c d)



map5 : (a -> b -> c -> d -> e -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe value
map5 func ma mb mc md me =
  case ma of
    Nothing ->
      Nothing

    Just a ->
      case mb of
        Nothing ->
          Nothing

        Just b ->
          case mc of
            Nothing ->
              Nothing

            Just c ->
              case md of
                Nothing ->
                  Nothing

                Just d ->
                  case me of
                    Nothing ->
                      Nothing

                    Just e ->
                      Just (func a b c d e)
