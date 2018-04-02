import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict
import Maybe
import Keyboard
import Task
import Window
import Animation
import Color exposing (..)

-- Define all styles I will use in my resume

concatStyles styles =
  style (List.concat styles)

boldText =
  [ ("font-weight" => "bold")
  ]

noBullet =
  [ ("list-style" => "none")
  ]

bulletPoint =
  [ ("list-style" => "circle")
  , ("line-height" => "24px")
  , ("letter-spacing" => "0")
  , ("margin-left" => "25px")
  ]

sectionTitle model =
  [ ("color" => (theming model Title))
  , ("font-size" => "25px")
  , ("line-height" => "48px")
  , ("margin" => "0px")
  ]

inlineBlock =
  [ ("display" => "inline-block")
  ]

block =
  [ ("display" => "block")
  ]

centerBlock =
  [ ("margin" => "0 auto")
  ]

overflowEllip =
  [ ("overflow" => "hidden")
  , ("text-overflow" => "ellipsis")
  , ("white-space" => "nowrap")
  ]

name =
  [ ("font-size" => "56px")
  , ("line-height" => "1.35")
  , ("letter-spacing" => "-.02em")
  , ("margin" => "0 0 24px 0")
  ] 
  
buttonContainerStyle =
  [ ("display" => "grid")
  , ("grid-template-columns" => "min-content min-content")
  , ("grid-column-gap" => "20px")
  , ("margin-top" => "15px")
  ]

buttonStyle themeType =
  [ ("background-color" => (buttonTheming themeType Primary))
  , ("border" => "1px solid")
  , ("border-color" => (buttonTheming themeType Title))
  , ("border-radius" => "3px")
  , ("color" => (buttonTheming themeType Title))
  , ("cursor" => "pointer")
  , ("display" => "inline-block")
  , ("padding" => "5px 10px")
  ]

styles : { img : List ( String, String ) }
styles =
  {
    img =
      [ ("position", "fixed")
      , ("top","0")
      , ( "right","0")
      , ( "width", "300px" )
      , ( "border", "1px solid #337AB7")
      ]
  }

-- Define all colors I want to use in the resume

colors =
  { orange = "rgb(255, 165, 0)"
  , black = "rgb(0, 0, 0)"
  , marine = "rgb(27, 43, 52)"
  , white = "rgb(255, 255, 255)"
  , yellow = "rgb(250, 200, 99)"
  , blue = "rgb(155, 194, 207)"
  , red = "rgb(236, 95, 103)"
  }

-- Set up some default colors for my resume

defaultTheme =
  { primary = colors.white
  , secondary = colors.marine
  , tertiary = colors.orange
  , text = colors.marine
  , title = colors.orange
  }

-- Set up customized set of colors
-- for different visual effect
-- Such as "Daytime reading"
-- Or "Evening reading"

themes =
  Dict.fromList
    [ ( "light"
      , { primary = colors.white
        , secondary = colors.marine
        , tertiary = colors.orange
        , text = colors.marine
        , title = colors.blue
        }
      ) 
    , ( "dark"
      , { primary = colors.marine
        , secondary = colors.orange
        , tertiary = colors.yellow
        , text = colors.white
        , title = colors.red
        }
      )
    ]

(=>) : a -> b -> (a, b)
(=>) = (,)

-- Create a model for my website

type alias Model =
  { code : Int
  , height : Int
  , width : Int
  , theme : String
  , style : Animation.State
  }

type Msg 
  = KeyMsg Keyboard.KeyCode
  | ResizeMsg {height: Int, width: Int}
  | ThemeMsg String
  | AnimateMsg Animation.Msg

type ThemingMsg 
  = Primary
  | Secondary
  | Tertiary
  | Title
  | Text


getInitialWindowDimensions : Cmd Msg
getInitialWindowDimensions =
  Task.perform ResizeMsg Window.size

-- Initialization

init : ( Model, Cmd Msg )
init = 
  ( { code = 0
    , height = 0
    , width = 0
    , theme = "theme1"
    , style =
      Animation.style
        [ Animation.backgroundColor (rgb 255 255 255)
        ] 
    }
  , getInitialWindowDimensions
  )

-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  case msg of 
    KeyMsg code -> 
      ( { model | code = code }
      , Cmd.none
      )
    ResizeMsg { height, width } ->
      ( { model | height = height, width = width}
      , Cmd.none
      )
    ThemeMsg theme ->
      ( { model | 
          theme = theme,
          style = Animation.interrupt
            [ Animation.to
              [ Animation.backgroundColor (if theme == "light" then rgb 255 255 255 else rgb 27 43 52)
              ]
            ]
            model.style
        }
      , Cmd.none
      )
    AnimateMsg msg ->
      ( { model | style = Animation.update msg model.style }
      , Cmd.none
      )


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.batch
    [ Keyboard.downs KeyMsg
    , Window.resizes ResizeMsg
    , Animation.subscription AnimateMsg [ model.style ]
    ]

-- Main function

main =
  Html.program 
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions 
    }




-- HELPERS

bigOrSmall model list1 list2 =
  if model.width > 1100 then
    list1
  else 
    list2

theming : Model -> ThemingMsg -> String
theming model msg =
  case msg of 
    Primary ->
      .primary <| Maybe.withDefault defaultTheme <| Dict.get model.theme themes
    Secondary ->
      .secondary <| Maybe.withDefault defaultTheme <| Dict.get model.theme themes
    Tertiary ->
      .tertiary <| Maybe.withDefault defaultTheme <| Dict.get model.theme themes
    Title ->
      .title <| Maybe.withDefault defaultTheme <| Dict.get model.theme themes
    Text ->
      .text <| Maybe.withDefault defaultTheme <| Dict.get model.theme themes

buttonTheming : String -> ThemingMsg -> String
buttonTheming themeType msg =
  case msg of 
    Primary ->
      .primary <| Maybe.withDefault defaultTheme <| Dict.get (String.toLower themeType) themes
    Secondary ->
      .secondary <| Maybe.withDefault defaultTheme <| Dict.get (String.toLower themeType) themes
    Tertiary ->
      .tertiary <| Maybe.withDefault defaultTheme <| Dict.get (String.toLower themeType) themes
    Title ->
      .title <| Maybe.withDefault defaultTheme <| Dict.get (String.toLower themeType) themes
    Text ->
      .text <| Maybe.withDefault defaultTheme <| Dict.get themeType themes


-- VIEW

view model =
  div 
    (bigOrSmall model 
      ( Animation.render model.style
      ++  [ style
            [ ("color" => (theming model Text))
            , ("font-family" => "Verdana")
            , ("display" => "grid")
            , ("grid-template-columns" => "15% minmax(auto, 800px)")
            , ("min-height" => "100vh")
            , ("padding" => "20px")
            ]
          ]
      )
      ( List.concat
        [ Animation.render model.style
        , [ style
            [ ("color" => (theming model Text))
            , ("font-family" => "Verdana")
            , ("min-height" => "100vh")
            , ("padding" => "5px")
            ]
          ]
        ]
      )
    )
    (bigOrSmall model 
      [ leftBar model
      , resumeView model
      ] 
      [ resumeView model
      ]
    )

-- Leftbar view model

leftBar model =
  div 
    [ style 
      [ ("border-left" => "2px solid ")
      , ("border-right" => "2px solid")
      , ("border-color" => (theming model Primary))
      , ("display" => "flex")
      , ("justify-content" => "center")
      , ("padding" => "50px 8px")
      , ("overflow-wrap" => "break-word")
      ]
    ]
    [ div 
      [ style 
        [ ("display" => "flex")
        , ("flex-flow" => "column")
        , ("max-width" => "100%")
        ]
      ]
      [ 
        img [src "https://b-ssl.duitang.com/uploads/item/201310/01/20131001200225_TKyjS.gif", style styles.img] []
      , span [ style inlineBlock ]
        [ text "My Elm Application for Assignment#2" ]
      , strong [ concatStyles [ inlineBlock, boldText ] ]
        [ a [herf"https://github.com/shens12/CS1XA3/tree/master/Assign2"] [text"daytime.elm"] ]
      , pre [] []
      , span [ style inlineBlock ]
        [ text "My Github Repository" ]
      , strong [ concatStyles [ inlineBlock, boldText ] ]
        [ a [href "https://github.com/shens12/CS1XA3"] 
            [text "Click here to see my github"]
        ]
      , pre [] []
      , span [style inlineBlock]
        [ text "Wikipedia link of related schools :"]
      , li [] [a [href "https://en.wikipedia.org/wiki/McMaster_University"]
                 [ text "McMaster University"]
              ]
      , pre [] []
      , li [] [a [href "https://baike.baidu.com/item/%E6%96%B0%E4%BC%9A%E7%AC%AC%E4%B8%80%E4%B8%AD%E5%AD%A6%E5%A5%8E%E5%B3%B0%E6%A0%A1%E5%8C%BA?fromtitle=%E8%91%B5%E5%9F%8E%E4%B8%AD%E5%AD%A6&fromid=7334471"]
                 [ text "Xinhui NO.1 Junior High School"]
              ]
      , pre [] []
      , li [] [a [href "https://baike.baidu.com/item/%E6%B1%9F%E9%97%A8%E5%B8%82%E6%96%B0%E4%BC%9A%E7%AC%AC%E4%B8%80%E4%B8%AD%E5%AD%A6?fromtitle=%E6%96%B0%E4%BC%9A%E4%B8%80%E4%B8%AD&fromid=7295084"]
                 [ text "Xinhui NO.1 Senior High School"]
              ]
      , pre [] []
      , li [] [a [href "https://baike.baidu.com/item/%E5%9C%AD%E5%B3%B0%E5%B0%8F%E5%AD%A6/5766265?fr=aladdin"]
                 [ text "Xinhui Guifeng Primary School"]
              ]
      ]
    ]

-- Button Function of the webpage

buttonContainer model =
  div 
    [ style buttonContainerStyle ]
    [ themeButton "light"
    , themeButton "dark"
    ]

themeButton themeType =
  div 
    [ style <| buttonStyle themeType
    , onClick << ThemeMsg <| String.toLower <| themeType
    ]
    [ text themeType ]

-- Resume view model

resumeView model =
  div 
    [ style 
      [ ("font-size" => "14px")
      , ("padding" => "0 30px")
      ] 
    ]
    [ myName model
    , educationExperience model 
    , abilities model
    , workingExperience model
    , contact
    ]


myName model =
  section []
    [ h1 [ style name ]
      [ text "Shanghong Shen" ]
      ,h2 []
      [text "申尚弘 / Michael / なおひろ"]
      ,h3 []
      [text "Choose your preferred lightness here :"]
      ,pre [] []
      ,h3 []
      [text "Please choose dark mode if surroundings are dark"]
      , buttonContainer model
    ]

educationExperience model =
  section []
    [ h2 [ style << sectionTitle <| model ]
      [ text "Education Experience"
      ]
    , ul []
      [ li [ style noBullet ]
        [ span [ style [("font-weight" => "bold")] ] 
          [ text "Guifeng Primary School" ]
        , p []
          [ text "September 2004 - June 2010" 
          , ul [ style bulletPoint ]
            [ li []
              [ text "Learned basic knowledge of Math, Chinese and English"
              ]
            , li [] 
              [ text "Gained basic abilities to get along with other individuals"
              ]
            ]
          ]
        ]
      , li [ style noBullet ]
        [ span [ style [("font-weight" => "bold")] ] 
          [ text "Xinhui NO.1 Junior High School " ]
        , p []
          [ text "September 2010 - June 2013"
          , ul [ style bulletPoint ]
            [ li []
              [ text "Learned basic knowledge of physics, chemistry and biology"
              ]
            , li []
              [ text "Learned basic knowledge of history, Chinese politics and world geography"
              ]
            , li []
              [ text "Learned intermediate knowledge of Math, Chinese and English"
              ]

            ]
          ]
        ]
      , li [ style noBullet ]
        [ span [ style [("font-weight" => "bold")] ] 
          [ text "Xinhui NO.1 Senior High School " ]
        , p []
          [ text "September 2013 - June 2016"
          , ul [ style bulletPoint ]
            [ li []
              [ text "Learned advanced knowledge of physics, chemistry and biology"
              ]
            , li []
              [ text "Learned advanced knowledge of history, Chinese politics and world geography"
              ]
            , li []
              [ text "Learned advanced knowledge of Math, Chinese and English"
              ]

            ]
          ]
        ]
      , li [ style noBullet ]
        [ span [ style [("font-weight" => "bold")] ] 
          [ text "McMaster University" ]
        , p []
          [ text "September 2016 - Present"
          , ul [ style bulletPoint ]
            [ li []
              [ text "Year I : Bachelor of Technology - Biotech Stream"
              ]
            , li []
              [ text "Year II : Computer Science Co-op"
              ]
            ]
          ]
        ]
      ]
    ]

abilities model =
  section []
    [ h2 [ style << sectionTitle <| model ]
      [ text "Abilities"
      ]
    , ul [ style bulletPoint ]
      [ li []
        [ text "In-depth experience with C++ and python"
        ]
      , li []
        [ text "Extensive knowledge of Haskell and computational thinking"
        ]
      , li []
        [ text "Have a good command of English skills, including reading, writing, listening and speaking"
        ]
      , li []
        [ text "N1 proficiency of Japanese, including perfect reading, writing, listening and speaking"
        ]
      , li []
        [ text "Basic understanding of Italian, Spanish and Korean"
        ]
      , li []
        [ text "Excellent understanding of both Mandarin and Cantonese"
        ]
      ]
    ]

workingExperience model =
  section []
    [ h2 [ style << sectionTitle <| model ]
      [ text "Working Experience"
      ]
    , span [ style boldText ] 
      [ text "Teaching Assistant -- McMaster University" 
      ]
    , ul [ style bulletPoint ]
      [ li []
        [ text "Teaching Assistant of Japanese courses, Department of Linguistics and Languages" ]
      ]
    ]


contact =
  section [] [
  pre [] [],
  h3 [] [text"Contact Information"],
  div []
    [ p []
      [ span [ style block ]
        [ text "Where to find me "
        , strong [ concatStyles [ inlineBlock, boldText ] ]
          [ text "16 Stroud Road, Hamilton, ON, Canada" ]
        ]
      , span [ style block ]
        [ text "How to get in touch with me " 
        , strong [ concatStyles [ inlineBlock, boldText ] ]
          [ text "shens12@mcmaster.ca" ]
        ]
      , span [style block]
        [ text "How to talk with me "
        , strong [ concatStyles [inlineBlock, boldText] ]
          [ text "365-888-2466"]
        ]
      , span [style block]
        [ text "My Home Residence"
        , strong [ concatStyles [inlineBlock, boldText] ]
          [ text "Room 605, Cuihuju, Beian Rd, Huicheng, Xinhui, Guangdong, China"]
        ]
      ]
    ]
    ]







