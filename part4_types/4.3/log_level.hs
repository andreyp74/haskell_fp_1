{-
Определите тип записи, который хранит элементы лога. Имя конструктора должно совпадать с именем типа, и запись должна содержать три поля:
timestamp — время, когда произошло событие (типа UTCTime);
logLevel — уровень события (типа LogLevel);
message — сообщение об ошибке (типа String).
Определите функцию logLevelToString, возвращающую текстуальное представление типа LogLevel, и функцию logEntryToString, возвращающую 
текстуальное представление записи в виде:

<время>: <уровень>: <сообщение>

Для преобразование типа UTCTime в строку используйте функцию timeToString
-}

import Data.Time.Clock
import Data.Time.Format
import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info

data LogEntry = LogEntry { timestamp::UTCTime, logLevel::LogLevel, message::String }

logLevelToString :: LogLevel -> String
logLevelToString Error = "Error"
logLevelToString Warning = "Warning"
logLevelToString Info = "Info"
logLevelToString _ = error "Unknown log level!"

logEntryToString :: LogEntry -> String
logEntryToString entry = timeToString (timestamp entry) ++ ": " ++ logLevelToString (logLevel entry) ++ ": " ++  (message entry)
