package trivalibs.graphics.buffers

import trivalibs.graphics.shader.WGSLType
import trivalibs.bufferdata.StructArray

import scala.NamedTuple.AnyNamedTuple

// =============================================================================
// AttribLayout type class
//
// Maps a named tuple of shader types (e.g. (position: Vec2, color: Vec4))
// to a concrete buffer tuple (e.g. (Vec2Buffer, Vec4Buffer)) that StructArray
// can work with at compile time.
//
// Uses a given chain rather than match types so that type aliases of named
// tuples (which don't reduce in match types) are handled correctly via
// implicit search.
// =============================================================================

// Fields is a second type parameter so it appears concretely in signatures
trait AttribLayout[Attribs, Fields <: Tuple]

object AttribLayout:
  given [A <: AnyNamedTuple, F <: Tuple]
    => (h: AttribLayoutHelper[NamedTuple.DropNames[A], F])
    => AttribLayout[A, F] = new AttribLayout[A, F] {}

trait AttribLayoutHelper[T <: Tuple, Fields <: Tuple]

object AttribLayoutHelper:
  given AttribLayoutHelper[EmptyTuple, EmptyTuple] =
    new AttribLayoutHelper[EmptyTuple, EmptyTuple] {}

  given [H, Tail <: Tuple, HBuf <: Tuple, TailFields <: Tuple]
    => (wt: WGSLType[H] { type AttribBuffer = HBuf })
    => (rest: AttribLayoutHelper[Tail, TailFields])
    => AttribLayoutHelper[H *: Tail, HBuf *: TailFields] =
    new AttribLayoutHelper[H *: Tail, HBuf *: TailFields] {}

// =============================================================================
// Allocation helpers
//
// transparent inline lets the compiler propagate the concrete StructArray[F]
// type to the call site, so field access compiles correctly.
// =============================================================================

transparent inline def allocateAttribs[Attribs](count: Int): Any =
  import scala.compiletime.summonFrom
  summonFrom:
    case al: AttribLayout[Attribs, f] => StructArray.allocate[f](count)
