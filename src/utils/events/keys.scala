package trivalibs.utils.events

import scala.scalajs.js

// ---------------------------------------------------------------------------
// Shared input constants. `Key` values mirror the DOM `KeyboardEvent.code`
// string verbatim (physical-key codes), so the constant name equals its value.
// `opaque type = String/Int` keeps the JS output as a raw primitive (no enum).
// ---------------------------------------------------------------------------

opaque type Key = String
object Key:
  // Letters
  val KeyA: Key = "KeyA"
  val KeyB: Key = "KeyB"
  val KeyC: Key = "KeyC"
  val KeyD: Key = "KeyD"
  val KeyE: Key = "KeyE"
  val KeyF: Key = "KeyF"
  val KeyG: Key = "KeyG"
  val KeyH: Key = "KeyH"
  val KeyI: Key = "KeyI"
  val KeyJ: Key = "KeyJ"
  val KeyK: Key = "KeyK"
  val KeyL: Key = "KeyL"
  val KeyM: Key = "KeyM"
  val KeyN: Key = "KeyN"
  val KeyO: Key = "KeyO"
  val KeyP: Key = "KeyP"
  val KeyQ: Key = "KeyQ"
  val KeyR: Key = "KeyR"
  val KeyS: Key = "KeyS"
  val KeyT: Key = "KeyT"
  val KeyU: Key = "KeyU"
  val KeyV: Key = "KeyV"
  val KeyW: Key = "KeyW"
  val KeyX: Key = "KeyX"
  val KeyY: Key = "KeyY"
  val KeyZ: Key = "KeyZ"

  // Digits (top row)
  val Digit0: Key = "Digit0"
  val Digit1: Key = "Digit1"
  val Digit2: Key = "Digit2"
  val Digit3: Key = "Digit3"
  val Digit4: Key = "Digit4"
  val Digit5: Key = "Digit5"
  val Digit6: Key = "Digit6"
  val Digit7: Key = "Digit7"
  val Digit8: Key = "Digit8"
  val Digit9: Key = "Digit9"

  // Function keys
  val F1: Key = "F1"
  val F2: Key = "F2"
  val F3: Key = "F3"
  val F4: Key = "F4"
  val F5: Key = "F5"
  val F6: Key = "F6"
  val F7: Key = "F7"
  val F8: Key = "F8"
  val F9: Key = "F9"
  val F10: Key = "F10"
  val F11: Key = "F11"
  val F12: Key = "F12"

  // Arrows
  val ArrowUp: Key = "ArrowUp"
  val ArrowDown: Key = "ArrowDown"
  val ArrowLeft: Key = "ArrowLeft"
  val ArrowRight: Key = "ArrowRight"

  // Whitespace / editing / navigation
  val Space: Key = "Space"
  val Enter: Key = "Enter"
  val Tab: Key = "Tab"
  val Escape: Key = "Escape"
  val Backspace: Key = "Backspace"
  val Delete: Key = "Delete"
  val Insert: Key = "Insert"
  val Home: Key = "Home"
  val End: Key = "End"
  val PageUp: Key = "PageUp"
  val PageDown: Key = "PageDown"
  val CapsLock: Key = "CapsLock"

  // Modifiers
  val ShiftLeft: Key = "ShiftLeft"
  val ShiftRight: Key = "ShiftRight"
  val ControlLeft: Key = "ControlLeft"
  val ControlRight: Key = "ControlRight"
  val AltLeft: Key = "AltLeft"
  val AltRight: Key = "AltRight"
  val MetaLeft: Key = "MetaLeft"
  val MetaRight: Key = "MetaRight"

  // Symbols
  val Minus: Key = "Minus"
  val Equal: Key = "Equal"
  val BracketLeft: Key = "BracketLeft"
  val BracketRight: Key = "BracketRight"
  val Backslash: Key = "Backslash"
  val Semicolon: Key = "Semicolon"
  val Quote: Key = "Quote"
  val Backquote: Key = "Backquote"
  val Comma: Key = "Comma"
  val Period: Key = "Period"
  val Slash: Key = "Slash"

  // Numpad
  val Numpad0: Key = "Numpad0"
  val Numpad1: Key = "Numpad1"
  val Numpad2: Key = "Numpad2"
  val Numpad3: Key = "Numpad3"
  val Numpad4: Key = "Numpad4"
  val Numpad5: Key = "Numpad5"
  val Numpad6: Key = "Numpad6"
  val Numpad7: Key = "Numpad7"
  val Numpad8: Key = "Numpad8"
  val Numpad9: Key = "Numpad9"
  val NumpadAdd: Key = "NumpadAdd"
  val NumpadSubtract: Key = "NumpadSubtract"
  val NumpadMultiply: Key = "NumpadMultiply"
  val NumpadDivide: Key = "NumpadDivide"
  val NumpadDecimal: Key = "NumpadDecimal"
  val NumpadEnter: Key = "NumpadEnter"

  extension (k: Key) inline def code: String = k

opaque type PointerButton = Int
object PointerButton:
  // Values match `MouseEvent.button`: 0 = primary, 1 = middle, 2 = secondary.
  val Primary: PointerButton = 0
  val Middle: PointerButton = 1
  val Secondary: PointerButton = 2
  extension (b: PointerButton)
    inline def value: Int = b
    inline def key: String = b.toString

opaque type PointerGesture = String
object PointerGesture:
  val Down: PointerGesture = "down"
  val Up: PointerGesture = "up"
  val Move: PointerGesture = "move"
  val Drag: PointerGesture = "drag"
  val Hold: PointerGesture = "hold"
  extension (g: PointerGesture) inline def name: String = g
