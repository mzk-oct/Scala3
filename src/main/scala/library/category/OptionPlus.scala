package library.category

object OptionPlus:
  extension [T](left: Option[T])
    inline def &(right: Option[T]): Option[(T, T)] =
      (left, right) match
        case (None, _) => None
        case (_, None) => None
        case (Some(l), Some(r)) => Some((l, r))
    inline def |(right: Option[T]): Option[T] =
      left match
        case None => right
        case _ => left

    inline def choice(right: Option[T])(selector: (T, T) => T): Option[T] =
      (left & right).map((i, j) => selector(i, j)) | left | right