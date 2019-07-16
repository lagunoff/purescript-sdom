module SDOM.Elements where

import SDOM
import Data.Either (Either)

a
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
a = element "a"

a_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
a_ = element_ "a"

abbr
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
abbr = element "abbr"

abbr_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
abbr_ = element_ "abbr"

acronym
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
acronym = element "acronym"

acronym_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
acronym_ = element_ "acronym"

address
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
address = element "address"

address_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
address_ = element_ "address"

applet
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
applet = element "applet"

applet_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
applet_ = element_ "applet"

area
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
area = element "area"

area_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
area_ = element_ "area"

article
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
article = element "article"

article_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
article_ = element_ "article"

aside
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
aside = element "aside"

aside_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
aside_ = element_ "aside"

audio
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
audio = element "audio"

audio_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
audio_ = element_ "audio"

b
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
b = element "b"

b_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
b_ = element_ "b"

base
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
base = element "base"

base_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
base_ = element_ "base"

basefont
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
basefont = element "basefont"

basefont_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
basefont_ = element_ "basefont"

bdi
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
bdi = element "bdi"

bdi_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
bdi_ = element_ "bdi"

bdo
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
bdo = element "bdo"

bdo_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
bdo_ = element_ "bdo"

big
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
big = element "big"

big_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
big_ = element_ "big"

blockquote
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
blockquote = element "blockquote"

blockquote_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
blockquote_ = element_ "blockquote"

body
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
body = element "body"

body_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
body_ = element_ "body"

br
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
br = element "br"

br_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
br_ = element_ "br"

button
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
button = element "button"

button_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
button_ = element_ "button"

canvas
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
canvas = element "canvas"

canvas_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
canvas_ = element_ "canvas"

caption
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
caption = element "caption"

caption_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
caption_ = element_ "caption"

center
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
center = element "center"

center_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
center_ = element_ "center"

cite
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
cite = element "cite"

cite_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
cite_ = element_ "cite"

code
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
code = element "code"

code_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
code_ = element_ "code"

col
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
col = element "col"

col_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
col_ = element_ "col"

colgroup
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
colgroup = element "colgroup"

colgroup_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
colgroup_ = element_ "colgroup"

datalist
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
datalist = element "datalist"

datalist_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
datalist_ = element_ "datalist"

dd
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
dd = element "dd"

dd_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
dd_ = element_ "dd"

del
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
del = element "del"

del_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
del_ = element_ "del"

details
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
details = element "details"

details_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
details_ = element_ "details"

dfn
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
dfn = element "dfn"

dfn_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
dfn_ = element_ "dfn"

dialog
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
dialog = element "dialog"

dialog_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
dialog_ = element_ "dialog"

dir
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
dir = element "dir"

dir_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
dir_ = element_ "dir"

div
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
div = element "div"

div_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
div_ = element_ "div"

dl
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
dl = element "dl"

dl_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
dl_ = element_ "dl"

dt
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
dt = element "dt"

dt_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
dt_ = element_ "dt"

em
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
em = element "em"

em_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
em_ = element_ "em"

embed
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
embed = element "embed"

embed_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
embed_ = element_ "embed"

fieldset
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
fieldset = element "fieldset"

fieldset_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
fieldset_ = element_ "fieldset"

figcaption
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
figcaption = element "figcaption"

figcaption_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
figcaption_ = element_ "figcaption"

figure
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
figure = element "figure"

figure_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
figure_ = element_ "figure"

font
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
font = element "font"

font_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
font_ = element_ "font"

footer
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
footer = element "footer"

footer_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
footer_ = element_ "footer"

form
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
form = element "form"

form_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
form_ = element_ "form"

frame
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
frame = element "frame"

frame_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
frame_ = element_ "frame"

frameset
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
frameset = element "frameset"

frameset_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
frameset_ = element_ "frameset"

h1
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
h1 = element "h1"

h1_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
h1_ = element_ "h1"

head
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
head = element "head"

head_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
head_ = element_ "head"

header
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
header = element "header"

header_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
header_ = element_ "header"

hr
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
hr = element "hr"

hr_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
hr_ = element_ "hr"

html
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
html = element "html"

html_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
html_ = element_ "htmch"

i
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
i = element "i"

i_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
i_ = element_ "i"

iframe
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
iframe = element "iframe"

iframe_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
iframe_ = element_ "iframe"

img
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
img = element "img"

img_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
img_ = element_ "img"

input
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
input = element "input"

input_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
input_ = element_ "input"

ins
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
ins = element "ins"

ins_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
ins_ = element_ "ins"

kbd
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
kbd = element "kbd"

kbd_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
kbd_ = element_ "kbd"

label
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
label = element "label"

label_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
label_ = element_ "label"

legend
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
legend = element "legend"

legend_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
legend_ = element_ "legend"

li
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
li = element "li"

li_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
li_ = element_ "li"

link
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
link = element "link"

link_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
link_ = element_ "link"

main
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
main = element "main"

main_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
main_ = element_ "main"

map
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
map = element "map"

map_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
map_ = element_ "map"

mark
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
mark = element "mark"

mark_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
mark_ = element_ "mark"

menu
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
menu = element "menu"

menu_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
menu_ = element_ "menu"

menuitem
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
menuitem = element "menuitem"

menuitem_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
menuitem_ = element_ "menuitem"

meta
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
meta = element "meta"

meta_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
meta_ = element_ "meta"

meter
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
meter = element "meter"

meter_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
meter_ = element_ "meter"

nav
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
nav = element "nav"

nav_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
nav_ = element_ "nav"

noframes
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
noframes = element "noframes"

noframes_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
noframes_ = element_ "noframes"

noscript
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
noscript = element "noscript"

noscript_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
noscript_ = element_ "noscript"

object
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
object = element "object"

object_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
object_ = element_ "object"

ol
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
ol = element "ol"

ol_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
ol_ = element_ "ol"

optgroup
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
optgroup = element "optgroup"

optgroup_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
optgroup_ = element_ "optgroup"

option
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
option = element "option"

option_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
option_ = element_ "option"

output
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
output = element "output"

output_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
output_ = element_ "outpuch"

p
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
p = element "p"

p_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
p_ = element_ "p"

param
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
param = element "param"

param_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
param_ = element_ "param"

picture
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
picture = element "picture"

picture_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
picture_ = element_ "picture"

pre
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
pre = element "pre"

pre_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
pre_ = element_ "pre"

progress
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
progress = element "progress"

progress_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
progress_ = element_ "progresch"

q
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
q = element "q"

q_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
q_ = element_ "q"

rp
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
rp = element "rp"

rp_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
rp_ = element_ "rp"

rt
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
rt = element "rt"

rt_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
rt_ = element_ "rt"

ruby
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
ruby = element "ruby"

ruby_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
ruby_ = element_ "rubch"

s
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
s = element "s"

s_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
s_ = element_ "s"

samp
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
samp = element "samp"

samp_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
samp_ = element_ "samp"

script
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
script = element "script"

script_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
script_ = element_ "script"

section
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
section = element "section"

section_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
section_ = element_ "section"

select
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
select = element "select"

select_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
select_ = element_ "select"

small
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
small = element "small"

small_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
small_ = element_ "small"

source
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
source = element "source"

source_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
source_ = element_ "source"

span
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
span = element "span"

span_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
span_ = element_ "span"

strike
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
strike = element "strike"

strike_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
strike_ = element_ "strike"

strong
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
strong = element "strong"

strong_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
strong_ = element_ "strong"

style
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
style = element "style"

style_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
style_ = element_ "style"

sub
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
sub = element "sub"

sub_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
sub_ = element_ "sub"

summary
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
summary = element "summary"

summary_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
summary_ = element_ "summary"

sup
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
sup = element "sup"

sup_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
sup_ = element_ "sup"

table
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
table = element "table"

table_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
table_ = element_ "table"

tbody
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
tbody = element "tbody"

tbody_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
tbody_ = element_ "tbody"

td
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
td = element "td"

td_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
td_ = element_ "td"

template
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
template = element "template"

template_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
template_ = element_ "template"

textarea
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
textarea = element "textarea"

textarea_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
textarea_ = element_ "textarea"

tfoot
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
tfoot = element "tfoot"

tfoot_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
tfoot_ = element_ "tfoot"

th
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
th = element "th"

th_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
th_ = element_ "th"

thead
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
thead = element "thead"

thead_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
thead_ = element_ "thead"

time
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
time = element "time"

time_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
time_ = element_ "time"

title
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
title = element "title"

title_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
title_ = element_ "title"

tr
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
tr = element "tr"

tr_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
tr_ = element_ "tr"

track
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
track = element "track"

track_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
track_ = element_ "track"

tt
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
tt = element "tt"

tt_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
tt_ = element_ "tch"

u
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
u = element "u"

u_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
u_ = element_ "u"

ul
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
ul = element "ul"

ul_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
ul_ = element_ "ul"

var
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
var = element "var"

var_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
var_ = element_ "var"

video
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
video = element "video"

video_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
video_ = element_ "video"

wbr
  :: forall channel i o
   . Array (Attr i)
  -> Array (Handler i (Either channel (i -> o)))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
wbr = element "wbr"

wbr_
  :: forall channel i o
   . Array (SDOM channel i o)
  -> SDOM channel i o
wbr_ = element_ "wbr"
