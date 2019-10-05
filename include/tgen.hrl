-record(tgen, {
    module :: atom(),
    name   :: string() | binary(),
    sha    :: string() | binary(),
    path   :: file:filename(),
    dest   :: file:filename() | undefined
}).