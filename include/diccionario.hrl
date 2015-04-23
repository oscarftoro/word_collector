-record(wc_word,{
  title            :: word_name(),
  language         :: language_initials(),
  definition       :: word_definition(),
  status           :: pasive | active | undefined,
  priority         :: high | medium | low | undefined,
  examples         :: string() | undefined,
  locations   = [] :: [{datetime(),location()}] | undefined,
  photos      = [] :: [photos] | undefined,
  date_time        :: datetime(),
  available        :: boolean()
	
}).

-record(wc_language,{
        name               :: language_name(),
        initials           :: language_initials(),
        is_mother_language :: is_mother_language()
}).

-type word_name()          :: string().
-type word_definition()    :: string().
-type datetime()           :: {{integer(),integer(),integer()},{integer(),integer(),integer()}}.
-type language_name()      :: string() | undefined.
-type language_initials()  :: string().
-type is_mother_language() :: boolean().
-type language()           :: {language_name(),language_initials(),is_mother_language()}.

-type latitude()           :: float().
-type longitud()           :: float().
-type location()           :: {{latitude(),longitud(),string()}, datetime()} | undefined.
-type photos()             :: [path()].
-type path()               :: string().
