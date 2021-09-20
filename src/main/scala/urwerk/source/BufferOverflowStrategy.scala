package urwerk.source

enum BufferOverflowStrategy:
  case DropLatest
  case DropOldest
  case Error