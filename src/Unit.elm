module Unit exposing (..)


type UnitType
    = Magic
    | Swords
    | Guns


typeDamageModifier : UnitType -> UnitType -> Float
typeDamageModifier t1 t2 =
    case ( t1, t2 ) of
        ( Magic, Guns ) ->
            2.0

        ( Guns, Swords ) ->
            2.0

        ( Swords, Magic ) ->
            2.0

        ( Magic, Swords ) ->
            0.5

        ( Swords, Guns ) ->
            0.5

        ( Guns, Magic ) ->
            0.5

        ( Magic, Magic ) ->
            1.0

        ( Guns, Guns ) ->
            1.0

        ( Swords, Swords ) ->
            1.0


type Unit
    = Unit
        { attack : Int
        , defence : Int
        , health : Int
        , unitType : UnitType
        , minRange : Int
        , maxRange : Int
        , speed : Int
        }
