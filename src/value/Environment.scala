package value

import expression.Identifier

/**
 * Jedi Environment
 */
class Environment extends collection.mutable.HashMap[Identifier, Value]
