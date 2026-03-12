object Utils {
  // TODO: find a better data structure for environments
  // type Environment = collection.immutable.Map[String, ExpValue]
  
  // Change environment to list of mutable maps to allow for nested scopes (for task 3)
  type Environment = List[collection.mutable.Map[String, ExpValue]]

  // keep the environment for functions unchanged since its not related to task 3
  type FunctionEnvironment = collection.immutable.Map[String, ExpValue]

  def format(value: ExpValue): String = value match {
    // DO NOT change the format for existing data types!
    // We need it for testing!
    case ExpBoolean(true) => "True"
    case ExpBoolean(false) => "False"
    case ExpInteger(i) => i.toString
    case ExpList(list) => list.map(format).mkString("[", ", ", "]")
    case ExpString(str) => str
    case value => value.toString
  }
}
