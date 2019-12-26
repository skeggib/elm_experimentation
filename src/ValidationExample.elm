module ValidationExample exposing (main)

import Browser
import Html
import Html.Attributes
import Html.Events


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    { firstName = ""
    , firstNameStatus = validateFirstName ""
    , lastName = ""
    , lastNameStatus = validateLastName ""
    , age = ""
    , ageStatus = validateAge ""
    , favoriteNumber = ""
    , favoriteNumberStatus = validateFavoriteNumber ""
    }



-- MODEL


type alias Person =
    { firstName : String
    , lastName : String
    , age : Int
    , favoriteNumber : Int
    }


type alias Model =
    { firstName : String
    , firstNameStatus : FirstNameStatus
    , lastName : String
    , lastNameStatus : LastNameStatus
    , age : String
    , ageStatus : AgeStatus
    , favoriteNumber : String
    , favoriteNumberStatus : FavoriteNumberStatus
    }


type FirstNameStatus
    = ValidFirstName String
    | EmptyFirstName


validateFirstName : String -> FirstNameStatus
validateFirstName firstName =
    if String.length firstName > 0 then
        ValidFirstName firstName

    else
        EmptyFirstName


type LastNameStatus
    = ValidLastName String
    | EmptyLastName


validateLastName : String -> LastNameStatus
validateLastName lastName =
    if String.length lastName > 0 then
        ValidLastName lastName

    else
        EmptyLastName


type AgeStatus
    = ValidAge Int
    | EmptyAge
    | NotANumberAge
    | InvalidAge


validateAge : String -> AgeStatus
validateAge ageStr =
    if String.length ageStr > 0 then
        case String.toInt ageStr of
            Just age ->
                if age >= 0 then
                    ValidAge age

                else
                    InvalidAge

            Nothing ->
                NotANumberAge

    else
        EmptyAge


type FavoriteNumberStatus
    = ValidFavoriteNumber Int
    | EmptyFavoriteNumber
    | NotANumberFavoriteNumber


validateFavoriteNumber : String -> FavoriteNumberStatus
validateFavoriteNumber favoriteNumberStr =
    if String.length favoriteNumberStr > 0 then
        case String.toInt favoriteNumberStr of
            Just favoriteNumber ->
                ValidFavoriteNumber favoriteNumber

            Nothing ->
                NotANumberFavoriteNumber

    else
        EmptyFavoriteNumber


validatePerson : Model -> Result Model Person
validatePerson formPerson =
    let
        firstNameStatus =
            validateFirstName formPerson.firstName

        lastNameStatus =
            validateLastName formPerson.lastName

        ageStatus =
            validateAge formPerson.age

        favoriteNumberStatus =
            validateFavoriteNumber formPerson.favoriteNumber

        errFormPerson =
            { formPerson
                | firstNameStatus = firstNameStatus
                , lastNameStatus = lastNameStatus
                , ageStatus = ageStatus
                , favoriteNumberStatus = favoriteNumberStatus
            }
    in
    case firstNameStatus of
        ValidFirstName firstName ->
            case lastNameStatus of
                ValidLastName lastName ->
                    case ageStatus of
                        ValidAge age ->
                            case favoriteNumberStatus of
                                ValidFavoriteNumber favoriteNumber ->
                                    Ok
                                        { firstName = firstName
                                        , lastName = lastName
                                        , age = age
                                        , favoriteNumber = favoriteNumber
                                        }

                                _ ->
                                    Err
                                        errFormPerson

                        _ ->
                            Err
                                errFormPerson

                _ ->
                    Err
                        errFormPerson

        _ ->
            Err
                errFormPerson



-- UPDATE


type Msg
    = UpdateFirstName String
    | UpdateLastName String
    | UpdateAge String
    | UpdateFavoriteNumber String
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateFirstName newFirstName ->
            { model | firstName = newFirstName }

        UpdateLastName newLastName ->
            { model | lastName = newLastName }

        UpdateAge newAge ->
            { model | age = newAge }

        UpdateFavoriteNumber newFavoriteNumber ->
            { model | favoriteNumber = newFavoriteNumber }

        Submit ->
            case validatePerson model of
                Ok _ ->
                    -- Submit the person
                    init

                Err errorWithStatus ->
                    errorWithStatus



-- VIEW


firstNameStatusToErrorMessage : FirstNameStatus -> String
firstNameStatusToErrorMessage status =
    case status of
        ValidFirstName _ ->
            ""

        EmptyFirstName ->
            "Please enter your first name"


lastNameStatusToErrorMessage : LastNameStatus -> String
lastNameStatusToErrorMessage status =
    case status of
        ValidLastName _ ->
            ""

        EmptyLastName ->
            "Please enter your last name"


ageStatusToErrorMessage : AgeStatus -> String
ageStatusToErrorMessage status =
    case status of
        ValidAge _ ->
            ""

        EmptyAge ->
            "Please enter your age"

        InvalidAge ->
            "Invalid age"

        NotANumberAge ->
            "This is not a number"


favoriteNumberStatusToErrorMessage : FavoriteNumberStatus -> String
favoriteNumberStatusToErrorMessage status =
    case status of
        ValidFavoriteNumber _ ->
            ""

        EmptyFavoriteNumber ->
            "Please enter your favorite number"

        NotANumberFavoriteNumber ->
            "This is not a number"


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.div []
            [ Html.input [ Html.Events.onInput UpdateFirstName, Html.Attributes.placeholder "First name", Html.Attributes.value model.firstName ] []
            , Html.text (firstNameStatusToErrorMessage model.firstNameStatus)
            ]
        , Html.div []
            [ Html.input [ Html.Events.onInput UpdateLastName, Html.Attributes.placeholder "Last name", Html.Attributes.value model.lastName ] []
            , Html.text (lastNameStatusToErrorMessage model.lastNameStatus)
            ]
        , Html.div []
            [ Html.input [ Html.Events.onInput UpdateAge, Html.Attributes.placeholder "Age", Html.Attributes.value model.age ] []
            , Html.text (ageStatusToErrorMessage model.ageStatus)
            ]
        , Html.div []
            [ Html.input [ Html.Events.onInput UpdateFavoriteNumber, Html.Attributes.placeholder "favoriteNumber", Html.Attributes.value model.favoriteNumber ] []
            , Html.text (favoriteNumberStatusToErrorMessage model.favoriteNumberStatus)
            ]
        , Html.div [] [ Html.button [ Html.Events.onClick Submit ] [ Html.text "Submit" ] ]
        ]
