package scaml.parser

/**
  * Chomping controls how final line breaks and trailing empty lines are interpreted.
  */
sealed trait BlockChomping

/**
  * Stripping is specified by the “-” chomping indicator. In this case, the final line break and any trailing empty
  * lines are excluded from the scalar’s content.
  */
final case object Strip extends BlockChomping

/**
  * Clipping is the default behavior used if no explicit chomping indicator is specified. In this case, the final
  * line break character is preserved in the scalar’s content. However, any trailing empty lines are excluded from
  * the scalar’s content.
  */
final case object Clip extends BlockChomping

/**
  * Keeping is specified by the “+” chomping indicator. In this case, the final line break and any trailing empty
  * lines are considered to be part of the scalar’s content. These additional lines are not subject to folding.
  */
final case object Keep extends BlockChomping