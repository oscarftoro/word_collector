-record(wc_word,{
  title            :: word_name(),
  language         :: language_initials(),
  definition       :: word_definition(),
  status           :: pasive | active | undefined,
  priority         :: high | medium | low | undefined,
  examples         :: binary() | undefined,
  locations   = [] :: [{datetime(),location()}] | [],
  photos      = [] :: [photos] | [],
  date_time        :: datetime(),
  available        :: boolean()
	
}).

-record(wc_language,{
        name               :: language_name(),
        initials           :: language_initials(),
        is_mother_language :: is_mother_language()
}).

-type word_name()          :: binary().
-type word_definition()    :: binary().
-type datetime()           :: {{integer(),integer(),integer()},{integer(),integer(),integer()}}.
-type language_name()      :: binary() | undefined.
-type language_initials()  :: binary().
-type is_mother_language() :: boolean().
-type language()           :: {language_name(),language_initials(),is_mother_language()}.

-type latitude()           :: float().
-type longitud()           :: float().
-type location()           :: {{latitude(),longitud(),binary()}, datetime()} | undefined.
-type photos()             :: [path()].
-type path()               :: binary().
