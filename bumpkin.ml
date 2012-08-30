(*#use "topfind"*)
open Str 
open Netcgi (*#require "netcgi2"*)
let encode = Netencoding.Html.encode ~in_enc:`Enc_utf8 () 


open Printf 



(* don't print the decimal point if it's followed by zeros *)
let num_to_str x = 
  let eps = 0.00000001 in 
  if abs_float (x -. floor x) > eps then 
    string_of_float x 
  else 
    string_of_int (int_of_float x)


module Size = struct 
  type t = 
    | Px of int 
    | Em of float 
    | Cm of float 
    | Pt of float 
    | Prct of float

  let to_str = function 
    | Px x -> (string_of_int x) ^ "px"
    | Em x -> (num_to_str x) ^ "em" 
    | Cm x -> (num_to_str x) ^ "cm" 
    | Pt x -> (num_to_str x) ^ "pt"
    | Prct x -> (num_to_str x) ^ "%"
end
open Size 

module Color = struct

  type t = 
    | Black 
    | White 
    | Red 
    | Green 
    | Blue 
    | LightGray | Gray | DarkGray 
    | Orange
    | Color of string 

  let to_str = function 
    | Black -> "black"
    | White -> "white"
    | Green -> "green" 
    | Red -> "red"
    | Blue -> "blue"
    | LightGray -> "#aaa"
    | Gray -> "gray"
    | DarkGray -> "#444"
    | Orange -> "ff6103" 
    | Color s -> s 
end
open Color

module Border = struct
  type style = 
    | Dotted 
    | Dashed 
    | Solid 
    | Double 
  
  let style_to_str = function 
    | Dotted -> "dotted"
    | Dashed -> "dashed"
    | Solid -> "solid" 
    | Double -> "double" 

  type t = Size.t * style * Color.t 
  let to_str (sz, st, c) = 
    sprintf "%s %s %s" (Size.to_str sz) (style_to_str st) (Color.to_str c)  
end
open Border

module Font = struct
  type style = Normal | Italic | Oblique 

  let style_to_str = function 
    | Normal -> "normal"
    | Italic -> "italic"
    | Oblique -> "oblique"

  type weight = 
    | Bold
    | Bolder
    | Lighter
    | Weight of int 

  let weight_to_str = function 
    | Bold -> "bold"
    | Bolder -> "bolder"
    | Lighter -> "lighter" 
    | Weight x -> string_of_int x 

  type family = 
    | Serif
    | SansSerif
    | Cursive
    | Fantasy
    | Monospace
    | Arial
    | Courier
    | Helvetica
    | Impact
    | Georgia
    | Verdana
    | Family of string 

  let family_to_str = function 
    | Serif -> "serif"
    | SansSerif -> "sans-serif" 
    | Cursive -> "cursive"
    | Fantasy -> "fantasy" 
    | Monospace -> "monospace"
    | Arial -> "arial"
    | Courier -> "courier"
    | Helvetica -> "helvetica"
    | Impact -> "impact"
    | Georgia -> "georgia"
    | Verdana -> "verdana" 
    | Family s -> s 
end


module Tag = struct 
  type t =
    | H1 | H2 | H3 | H4 | H5 | H6
    | P 
    | Strong
    | Em 
    | Img  
    | Div
    | Center
    | DefList
    | DefTerm
    | DefDesc
    | Ol
    | Ul 
    | Li
    | Pre
    | Br  

  let to_str = function 
    | H1 -> "h1"
    | H2 -> "h2"
    | H3 -> "h3"
    | H4 -> "h4"
    | H5 -> "h5"
    | H6 -> "h6" 
    | P -> "p"
    | Strong -> "strong"
    | Em -> "em"
    | Img -> "img"
    | Div -> "div"
    | Center -> "center"
    | DefList -> "dl"
    | DefTerm -> "dt"
    | DefDesc -> "dd"
    | Ol -> "ol"
    | Ul -> "ul"
    | Li -> "li"
    | Pre -> "pre"
    | Br -> "br" 

  let is_block = function 
    | H1 | H2 | H3 | H4 | H5 | H6 
    | P 
    | Div 
    | Ol | Ul | DefList
    | Pre 
    | Br -> true 
    | _ -> false 
end
open Tag 

module Css = struct 
 
  type selector = 
   | Id of string 
   | Class of string
   | Tag of Tag.t 
   | Link of selector
   | Visited of selector 

  let rec selector_to_str = function 
   | Id s -> "#" ^ s
   | Class s -> "." ^ s
   | Link x -> (selector_to_str x) ^ ":link"
   | Visited x -> (selector_to_str x) ^ ":visited" 
   | Tag t -> Tag.to_str t 

  type 'a sides = { top : 'a; bottom: 'a; left: 'a; right: 'a} 
  let sides_to_str {top; bottom; left; right} nested_to_str = 
    String.concat " " (List.map nested_to_str [top; right; bottom; left])

  type property = 
    (* padding *) 
    | Padding of Size.t sides 
    | PaddingLeft of Size.t 
    | PaddingRight of Size.t
    | PaddingTop of Size.t
    | PaddingBottom of Size.t
    (* border *) 
    | Border of Border.t sides 
    | BorderTop of Border.t
    | BorderBottom of Border.t 
    | BorderLeft of Border.t
    | BorderRight of Border.t
    (* colors *)
    | BackgroundColor of Color.t 
    | Color of Color.t 
    | Opacity of float
    (* dimensions *) 
    | Height of Size.t 
    | Width of Size.t
    | MaxWidth of Size.t 
    | MinWidth of Size.t 
    | MaxHeight of Size.t
    | MinHeight of Size.t 
    (* fonts *) 
    | FontStyle of Font.style 
    | FontSize of Size.t 
    | FontFamily of Font.family list 

  let property_to_str = function 
    | Padding sides -> "padding: " ^ (sides_to_str sides Size.to_str)
    | PaddingLeft sz -> "padding-left: " ^ (Size.to_str sz)
    | PaddingRight sz -> "padding-right: " ^ (Size.to_str sz)
    | PaddingTop sz -> "padding-top: " ^ (Size.to_str sz)
    | PaddingBottom sz -> "padding-bottom: " ^ (Size.to_str sz)
    | Border sides -> "border: " ^ (sides_to_str sides Border.to_str)
    | BorderTop b -> "border-top: " ^ (Border.to_str b)
    | BorderBottom b -> "border-bottom: " ^ (Border.to_str b)
    | BorderLeft b -> "border-left: " ^ (Border.to_str b)
    | BorderRight b -> "border-right: " ^ (Border.to_str b)
    | BackgroundColor c -> "background-color: " ^ (Color.to_str c)
    | Color c -> "color: " ^ (Color.to_str c)
    | Opacity level -> "opacity: " ^ (string_of_float level) 
    | Height sz -> "height: " ^ (Size.to_str sz)
    | Width sz -> "width: " ^ (Size.to_str sz)
    | MaxWidth sz -> "max-width: " ^ (Size.to_str sz)
    | MinWidth sz -> "min-width: " ^ (Size.to_str sz)
    | MaxHeight sz -> "max-height: " ^ (Size.to_str sz)
    | MinHeight sz -> "min-height: " ^ (Size.to_str sz)
    | FontStyle st -> "font-style: " ^ (Font.style_to_str st)
    | FontSize sz -> "font-size: " ^ (Size.to_str sz)
    | FontFamily families -> 
      "font-family: " ^ 
        (String.concat ", " (List.map Font.family_to_str families))
 
  type entry = selector * (property list)
  
  let entry_to_str (s, props) = 
    let prop_strings = List.map property_to_str props in 
    sprintf " %s { %s\n}" (selector_to_str s) (String.concat "\n\t" prop_strings)

  type t = entry list 

  let css_to_str entries = 
    String.concat "\n\n" (List.map entry_to_str entries)
end
open Css 


module Elt = struct 
  type attr = 
    | Src of string

  let attr_to_str = function 
    | Src s -> sprintf "src = \"%s\"" (encode s)
   
  type tag_elt = {
    tag : Tag.t; 
    id : string option; 
    classes : string list;  
    attrs : attr list; 
    children : t list;  
  } 
  and t = 
   | Text of string 
   | TagElt of tag_elt
  
  
  let rec tag_elt_to_str ?(depth=1) {tag; id; classes; attrs; children} = 
    let tag_name = Tag.to_str tag in 
    let id_str = match id with 
      | None -> "" 
      | Some id -> sprintf "id = \"%s\"" (encode id) 
    in 
    let classes_str : string = match classes with 
      | [] -> ""
      | _ -> String.concat " " (List.map encode classes) 
    in 
    let attrs_str : string = match attrs with  
      | [] -> ""
      | _ -> " " ^ String.concat " " (List.map attr_to_str attrs)
    in 
    let children_str : string = match children with 
      | [] -> ""
      | _ -> String.concat " " (List.map (to_str ~depth:(depth+1)) children) 
    in 
    let prefix : string = String.make (2*depth) ' ' in 
    let start_tag : string = sprintf "<%s%s%s%s>" tag_name id_str classes_str attrs_str in 
    let end_tag : string = sprintf "</%s>" tag_name in 
    prefix ^ start_tag ^ children_str ^ end_tag

  and to_str ?(depth=1) = function 
    | Text s -> encode s 
    | TagElt elt ->
      let newline = if Tag.is_block elt.tag then "\n" else "" in 
      newline ^ (tag_elt_to_str ~depth elt) ^ newline 
end 
open Elt 

module Helpers = struct
 
  let img ?(id=None) ?(cl =[]) src = 
    TagElt { tag = Img; id = id; classes = cl; attrs = [Src src]; children = [] }
  
  let div ?id ?(cl=[]) nested = 
    TagElt { tag = Div; id = id; classes = cl; attrs = []; children = nested }

  let center ?id ?(cl=[]) nested = 
    TagElt { tag = Center; id = id; classes = cl; attrs = []; children = nested } 
  
  let br = 
    TagElt { tag = Br; id = None; classes = []; attrs = []; children = [] } 

  let text s = Text s
  let __ s = Text s 

end
open Helpers 

type doc = { 
  title : string; 
  body : Elt.t list; 
}

  
let doc_to_str {title; body} : string = 
  let body_str : string = String.concat "\n\n" (List.map Elt.to_str body) in 
  sprintf  "<html>\n<head><title>%s</title></head>\n<body>\n%s\n</body>\n</html>" title body_str 

let run doc = print_string (doc_to_str doc)

let test_img : Elt.t = img "test.jpg"
let test_txt : Elt.t = __ "Hello, this is a penguin" 
let test_center : Elt.t = center [test_img; br; test_txt]
let test_div = div [test_center] 
let test_doc = { title = "THIS IS A TEST"; body = [test_div] } 

let _ = run test_doc 
