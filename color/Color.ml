type ('a, 'b) result =
	| Ok of 'a
  | Err of 'b

type t = Blue | Green | Red | White | Yellow | Cyan | Magenta

let toTermString t = match t with
  | Red -> "\x1b[31m"
  | Green -> "\x1b[32m"
  | Yellow -> "\x1b[33m"
  | Blue -> "\x1b[34m"
  | Magenta -> "\x1b[35m"
  | Cyan -> "\x1b[36m"
  | White -> "\x1b[00m"

let applyColorToString t s = toTermString t ^ s ^ toTermString White

let toString t = match t with
  | Red -> applyColorToString Red "Red"
  | Green -> applyColorToString Green "Green"
  | Yellow -> applyColorToString Yellow "Yellow"
  | Blue -> applyColorToString Blue "Blue"
  | Magenta -> applyColorToString Magenta "Magenta"
  | Cyan -> applyColorToString Cyan "Cyan"
  | White -> applyColorToString White "White"

let parseString s = match String.lowercase (String.trim s) with
  | "red" | "r" -> Ok Red
  | "green" | "g" -> Ok Green
  | "yellow" | "y" -> Ok Yellow
  | "blue" | "b" -> Ok Blue
  | "magenta" | "m" -> Ok Magenta
  | "cyan" | "c" -> Ok Cyan
  | "white" | "w" -> Ok White
  | invalid -> Err ("Wrong color name : \"" ^ invalid ^ "\"")
