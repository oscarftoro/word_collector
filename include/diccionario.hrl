-record(word,{
        id          :: integer(),
        name        :: string(),
        definition  :: string(),
        status      :: pasive | active,
        examples    :: string(),
        location    :: [location()],
        photos      :: [photos]
}).
-record(language,{
        name               :: string(),
        initials           :: string(),
        is_mother_language :: boolean()
}).


-type latitude() :: float().
-type longitud() :: float().
-type location() :: {latitude(),longitud(),string()}.
-type photos()   :: [string()].