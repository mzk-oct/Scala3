case class Diff(
                 name: String
               )

case class DuplicateData(
                          sameId: Int,
                          differentVal: Diff
                        )

val list = Seq(
  DuplicateData(1, Diff("a")),
  DuplicateData(1, Diff("b")),
  DuplicateData(1, Diff("c")),
  DuplicateData(1, Diff("d")),
  DuplicateData(1, Diff("e"))
)

list.groupMap(_.sameId)(_.differentVal)