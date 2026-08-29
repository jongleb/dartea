{- Vendored by dartea, an independent compiler. Not affiliated with or
   endorsed by the Elm project.

   Derived from elm/html -- https://github.com/elm/html
   Copyright (c) 2014-present, Evan Czaplicki, BSD 3-Clause License.
   dartea's LICENSE carries the full text and the file-by-file list.
-}


module Html exposing
    ( Attribute, Html, a, abbr, address, article, aside, audio, b, bdi, bdo
    , blockquote, br, button, canvas, caption, cite, code, col, colgroup
    , datalist, dd, del, details, dfn, div, dl, dt, em, embed, fieldset
    , figcaption, figure, footer, form, h1, h2, h3, h4, h5, h6, header, hr, i
    , iframe, img, input, ins, kbd, label, legend, li, main_, map, mark, math
    , menu, menuitem, meter, nav, node, object, ol, optgroup, option, output
    , p, param, pre, progress, q, rp, rt, ruby, s, samp, section, select
    , small, source, span, strong, sub, summary, sup, table, tbody, td, text
    , textarea, tfoot, th, thead, time, tr, track, u, ul, var, video, wbr
    )

import VirtualDom


type alias Html msg =
    VirtualDom.Node msg


type alias Attribute msg =
    VirtualDom.Attribute msg


node : String -> List (Attribute msg) -> List (Html msg) -> Html msg
node =
    VirtualDom.node


map : (a -> msg) -> Html a -> Html msg
map =
    VirtualDom.map


text : String -> Html msg
text =
    VirtualDom.text


a : List (Attribute msg) -> List (Html msg) -> Html msg
a =
    node "a"


abbr : List (Attribute msg) -> List (Html msg) -> Html msg
abbr =
    node "abbr"


address : List (Attribute msg) -> List (Html msg) -> Html msg
address =
    node "address"


article : List (Attribute msg) -> List (Html msg) -> Html msg
article =
    node "article"


aside : List (Attribute msg) -> List (Html msg) -> Html msg
aside =
    node "aside"


audio : List (Attribute msg) -> List (Html msg) -> Html msg
audio =
    node "audio"


b : List (Attribute msg) -> List (Html msg) -> Html msg
b =
    node "b"


bdi : List (Attribute msg) -> List (Html msg) -> Html msg
bdi =
    node "bdi"


bdo : List (Attribute msg) -> List (Html msg) -> Html msg
bdo =
    node "bdo"


blockquote : List (Attribute msg) -> List (Html msg) -> Html msg
blockquote =
    node "blockquote"


br : List (Attribute msg) -> List (Html msg) -> Html msg
br =
    node "br"


button : List (Attribute msg) -> List (Html msg) -> Html msg
button =
    node "button"


canvas : List (Attribute msg) -> List (Html msg) -> Html msg
canvas =
    node "canvas"


caption : List (Attribute msg) -> List (Html msg) -> Html msg
caption =
    node "caption"


cite : List (Attribute msg) -> List (Html msg) -> Html msg
cite =
    node "cite"


code : List (Attribute msg) -> List (Html msg) -> Html msg
code =
    node "code"


col : List (Attribute msg) -> List (Html msg) -> Html msg
col =
    node "col"


colgroup : List (Attribute msg) -> List (Html msg) -> Html msg
colgroup =
    node "colgroup"


datalist : List (Attribute msg) -> List (Html msg) -> Html msg
datalist =
    node "datalist"


dd : List (Attribute msg) -> List (Html msg) -> Html msg
dd =
    node "dd"


del : List (Attribute msg) -> List (Html msg) -> Html msg
del =
    node "del"


details : List (Attribute msg) -> List (Html msg) -> Html msg
details =
    node "details"


dfn : List (Attribute msg) -> List (Html msg) -> Html msg
dfn =
    node "dfn"


div : List (Attribute msg) -> List (Html msg) -> Html msg
div =
    node "div"


dl : List (Attribute msg) -> List (Html msg) -> Html msg
dl =
    node "dl"


dt : List (Attribute msg) -> List (Html msg) -> Html msg
dt =
    node "dt"


em : List (Attribute msg) -> List (Html msg) -> Html msg
em =
    node "em"


embed : List (Attribute msg) -> List (Html msg) -> Html msg
embed =
    node "embed"


fieldset : List (Attribute msg) -> List (Html msg) -> Html msg
fieldset =
    node "fieldset"


figcaption : List (Attribute msg) -> List (Html msg) -> Html msg
figcaption =
    node "figcaption"


figure : List (Attribute msg) -> List (Html msg) -> Html msg
figure =
    node "figure"


footer : List (Attribute msg) -> List (Html msg) -> Html msg
footer =
    node "footer"


form : List (Attribute msg) -> List (Html msg) -> Html msg
form =
    node "form"


h1 : List (Attribute msg) -> List (Html msg) -> Html msg
h1 =
    node "h1"


h2 : List (Attribute msg) -> List (Html msg) -> Html msg
h2 =
    node "h2"


h3 : List (Attribute msg) -> List (Html msg) -> Html msg
h3 =
    node "h3"


h4 : List (Attribute msg) -> List (Html msg) -> Html msg
h4 =
    node "h4"


h5 : List (Attribute msg) -> List (Html msg) -> Html msg
h5 =
    node "h5"


h6 : List (Attribute msg) -> List (Html msg) -> Html msg
h6 =
    node "h6"


header : List (Attribute msg) -> List (Html msg) -> Html msg
header =
    node "header"


hr : List (Attribute msg) -> List (Html msg) -> Html msg
hr =
    node "hr"


i : List (Attribute msg) -> List (Html msg) -> Html msg
i =
    node "i"


iframe : List (Attribute msg) -> List (Html msg) -> Html msg
iframe =
    node "iframe"


img : List (Attribute msg) -> List (Html msg) -> Html msg
img =
    node "img"


input : List (Attribute msg) -> List (Html msg) -> Html msg
input =
    node "input"


ins : List (Attribute msg) -> List (Html msg) -> Html msg
ins =
    node "ins"


kbd : List (Attribute msg) -> List (Html msg) -> Html msg
kbd =
    node "kbd"


label : List (Attribute msg) -> List (Html msg) -> Html msg
label =
    node "label"


legend : List (Attribute msg) -> List (Html msg) -> Html msg
legend =
    node "legend"


li : List (Attribute msg) -> List (Html msg) -> Html msg
li =
    node "li"


main_ : List (Attribute msg) -> List (Html msg) -> Html msg
main_ =
    node "main"


mark : List (Attribute msg) -> List (Html msg) -> Html msg
mark =
    node "mark"


math : List (Attribute msg) -> List (Html msg) -> Html msg
math =
    node "math"


menu : List (Attribute msg) -> List (Html msg) -> Html msg
menu =
    node "menu"


menuitem : List (Attribute msg) -> List (Html msg) -> Html msg
menuitem =
    node "menuitem"


meter : List (Attribute msg) -> List (Html msg) -> Html msg
meter =
    node "meter"


nav : List (Attribute msg) -> List (Html msg) -> Html msg
nav =
    node "nav"


object : List (Attribute msg) -> List (Html msg) -> Html msg
object =
    node "object"


ol : List (Attribute msg) -> List (Html msg) -> Html msg
ol =
    node "ol"


optgroup : List (Attribute msg) -> List (Html msg) -> Html msg
optgroup =
    node "optgroup"


option : List (Attribute msg) -> List (Html msg) -> Html msg
option =
    node "option"


output : List (Attribute msg) -> List (Html msg) -> Html msg
output =
    node "output"


p : List (Attribute msg) -> List (Html msg) -> Html msg
p =
    node "p"


param : List (Attribute msg) -> List (Html msg) -> Html msg
param =
    node "param"


pre : List (Attribute msg) -> List (Html msg) -> Html msg
pre =
    node "pre"


progress : List (Attribute msg) -> List (Html msg) -> Html msg
progress =
    node "progress"


q : List (Attribute msg) -> List (Html msg) -> Html msg
q =
    node "q"


rp : List (Attribute msg) -> List (Html msg) -> Html msg
rp =
    node "rp"


rt : List (Attribute msg) -> List (Html msg) -> Html msg
rt =
    node "rt"


ruby : List (Attribute msg) -> List (Html msg) -> Html msg
ruby =
    node "ruby"


s : List (Attribute msg) -> List (Html msg) -> Html msg
s =
    node "s"


samp : List (Attribute msg) -> List (Html msg) -> Html msg
samp =
    node "samp"


section : List (Attribute msg) -> List (Html msg) -> Html msg
section =
    node "section"


select : List (Attribute msg) -> List (Html msg) -> Html msg
select =
    node "select"


small : List (Attribute msg) -> List (Html msg) -> Html msg
small =
    node "small"


source : List (Attribute msg) -> List (Html msg) -> Html msg
source =
    node "source"


span : List (Attribute msg) -> List (Html msg) -> Html msg
span =
    node "span"


strong : List (Attribute msg) -> List (Html msg) -> Html msg
strong =
    node "strong"


sub : List (Attribute msg) -> List (Html msg) -> Html msg
sub =
    node "sub"


summary : List (Attribute msg) -> List (Html msg) -> Html msg
summary =
    node "summary"


sup : List (Attribute msg) -> List (Html msg) -> Html msg
sup =
    node "sup"


table : List (Attribute msg) -> List (Html msg) -> Html msg
table =
    node "table"


tbody : List (Attribute msg) -> List (Html msg) -> Html msg
tbody =
    node "tbody"


td : List (Attribute msg) -> List (Html msg) -> Html msg
td =
    node "td"


textarea : List (Attribute msg) -> List (Html msg) -> Html msg
textarea =
    node "textarea"


tfoot : List (Attribute msg) -> List (Html msg) -> Html msg
tfoot =
    node "tfoot"


th : List (Attribute msg) -> List (Html msg) -> Html msg
th =
    node "th"


thead : List (Attribute msg) -> List (Html msg) -> Html msg
thead =
    node "thead"


time : List (Attribute msg) -> List (Html msg) -> Html msg
time =
    node "time"


tr : List (Attribute msg) -> List (Html msg) -> Html msg
tr =
    node "tr"


track : List (Attribute msg) -> List (Html msg) -> Html msg
track =
    node "track"


u : List (Attribute msg) -> List (Html msg) -> Html msg
u =
    node "u"


ul : List (Attribute msg) -> List (Html msg) -> Html msg
ul =
    node "ul"


var : List (Attribute msg) -> List (Html msg) -> Html msg
var =
    node "var"


video : List (Attribute msg) -> List (Html msg) -> Html msg
video =
    node "video"


wbr : List (Attribute msg) -> List (Html msg) -> Html msg
wbr =
    node "wbr"
