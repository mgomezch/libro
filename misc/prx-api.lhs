> data Revocation
>   = Revocation
>     { revocationTimestamp ∷ UTCTime
>     , successor           ∷ Maybe Route
>     }
>
> data Version state
>   = Version
>     { predecessor      ∷ Maybe Route
>     , revocation       ∷ Maybe Revocation
>     , state            ∷ state
>     , versionTimestamp ∷ UTCTime
>     }

# Resources

Resource                                    Route
------------------------------------------- ------------------------------------------------------------------------------
Table collection                            `/v1/database`
Table column metadata collection            `/v1/database/«table code name»`
Column metadata                             `/v1/database/«table code name»/column/«column code name»`
Table active row collection                 `/v1/database/«table code name»/rows`
Table row version data                      `/v1/database/«table code name»/version/«row version identifier»`
Redirect, table row version data            `/v1/database/«table code name»/version/«row version identifier»/predecessor`
Redirect, table row version data            `/v1/database/«table code name»/version/«row version identifier»/successor`
Version-centric row history sequence        `/v1/database/«table code name»/version/«row version identifier»/history`
------------------------------------------- ------------------------------------------------------------------------------

# Entry point

    → GET /v1/database
    Accept: application/vnd.siren+json

    ← 200 OK
    { "links":
      [ { "rel": ["self"], "href": "http://api.pcaaudit.com/v1/database"            }
      , { "rel": ["item"], "href": "http://api.pcaaudit.com/v1/database/asociacion" }
      , { "rel": ["item"], "href": "http://api.pcaaudit.com/v1/database/variante"   }
      …
      ]
    }



# Table metadata


## Tables

Table metadata should **not** be used to drive application state.
It should be used purely as informative reference, perhaps to show
human-readable documentation inline with controls if it can’t
be placed at a better location, or to automatically generate
documentation, diagrams or other forms of metadata.  Specifically,
it should not be used to generate URIs — that’s what links and
actions are for.

    → GET /variante
    Accept: application/vnd.siren+json

    ← 200 OK
    { "properties":
      { "table name": "Variante de asociación farmacológica"
      , "code name": "variante"
      , "comment": "Forma distinguida de una asociación farmacológica según su aplicación terapéutica para grupos fisiológicos o patológicos particulares."
      }

    , "entities":
      [ { "rel": "item", "href": "http://api.pcaaudit.com/v1/database/variante/column/asociacion" }
      , { "rel": "item", "href": "http://api.pcaaudit.com/v1/database/variante/column/variante"   }
      , { "rel": "item", "href": "http://api.pcaaudit.com/v1/database/variante/column/dmd"        }
      ]

    , "links":
      [ { "rel": ["self"      ], "href": "http://api.pcaaudit.com/v1/database/variante"      }
      , { "rel": ["describes" ], "href": "http://api.pcaaudit.com/v1/database/variante/rows" }
      , { "rel": ["collection"], "href": "http://api.pcaaudit.com/v1/database"               }
      ]
    }


## Properties

Server-side validation will catch all errors and usability can be
postponed, so property types may temporarily be simplified to `text`.

Note:
Property type representations should include a type description that
allows for the generation of appropriate form controls with validation.
However, this will require support in the Siren browser.  This should
at least support references perhaps with some sort of autocompletion
UI, enumerated types with selectors, and rule ASTs with the Blockly
GUI.

    → GET /variante/column/asociacion

    ← 200 OK
    { "properties":
      { "comment": "Asociación farmacológica de la cual deriva la variante."
      }

    , "links":
      [ { "rel": ["self"      ], "href": "http://api.pcaaudit.com/v1/database/variante/column/variante" }
      , { "rel": ["collection"], "href": "http://api.pcaaudit.com/v1/database/variante"                 }
      ]
    }



# Collections

It should be possible to ask for embedded representations instead of
embedded links (as an optimization) with some request parameter.  There
should be stronger limits on the number of requested representations
in this case.  Something like `?embed` in the query string.

    → GET /variante/rows
    Accept: application/vnd.siren+json

    ← 302 Found
    Location: /variante/rows?offset=0&limit=5

    → GET /variante/rows?offset=0&limit=5
    Accept: application/vnd.siren+json

    ← 200 OK
    { "properties":
      { "count": 1337
      }

    , "entities":
      [ { "rel": "item", "href": "http://api.pcaaudit.com/v1/database/variante/version/3e7c1b67-6452-46b8-b0e0-690150ef9e0a" }
      , { "rel": "item", "href": "http://api.pcaaudit.com/v1/database/variante/version/53d11f52-3f2b-48f1-98d2-95a2135ae827" }
      , { "rel": "item", "href": "http://api.pcaaudit.com/v1/database/variante/version/45a1556d-50af-4dba-8430-d8fb840e10ae" }
      , { "rel": "item", "href": "http://api.pcaaudit.com/v1/database/variante/version/b5616572-c36c-42b5-b5df-c160d7908480" }
      , { "rel": "item", "href": "http://api.pcaaudit.com/v1/database/variante/version/6e9a49d4-0d4d-4ac6-85ed-71d6323db336" }
      ]

    , "links":
      [ { "rel": ["self"       ], "href": "http://api.pcaaudit.com/v1/database/variante/rows?offset=0&limit=5" }
      , { "rel": ["next"       ], "href": "http://api.pcaaudit.com/v1/database/variante/rows?offset=5&limit=5" }
      , { "rel": ["describedby"], "href": "http://api.pcaaudit.com/v1/database/variante"                       }
      ]

    , "actions":
      [ { "name": "insert"
        , "title": "Insert"
        , "method": "POST"
        , "href": "http://api.pcaaudit.com/v1/database/variante/rows"
        , "fields":
          [ { "name": "asociación farmacológica"
            , "type": "http://api.pcaaudit.com/v1/database/asociacion"
            }
          , { "name": "variante"
            , "type": "http://api.pcaaudit.com/v1/database/variante/column/variante"
            }
          , { "name": "dosis máxima diaria"
            , "type": "http://api.pcaaudit.com/v1/database/variante/column/dmd"
            }
          ]
        }

      , { "name": "limit"
        , "title": "Limit"
        , "method": "GET"
        , "href": "http://api.pcaaudit.com/v1/database/variante/rows"
        , "fields":
          [ { "name": "offset", "type": "number", "value": 0 }
          , { "name": "limit" , "type": "number", "value": 5 }
          ]
        }

      , { "name": "search"
        , "title": "Search"
        , "method": "GET"
        , "href": "http://api.pcaaudit.com/v1/database/variante/rows"
        , "fields":
          [ { "name": "asociación farmacológica"
            , "type": "http://api.pcaaudit.com/v1/database/variante/column/asociacion"
            }
          , { "name": "variante"
            , "type": "http://api.pcaaudit.com/v1/database/variante/column/variante"
            }
          , { "name": "dosis máxima diaria"
            , "type": "http://api.pcaaudit.com/v1/database/variante/column/dmd"
            }
          ]
        }
      ]
    }



# Resource states

Note:
This example has a successor version and a revocation timestamp, so
it’s been succeeded by an update.  Therefore, it does not make sense
for deletion to be an available action.  It’s provided solely as an
example of how deletion actions would look.

    → GET /variante/version/c4462cf1-33d8-4123-8a05-a2ee318ff141
    Accept: application/vnd.siren+json

    ← 200 OK
    { "properties":
      { "asociación farmacológica": "http://api.pcaaudit.com/v1/database/asociacion/version/44bc82d6-b1da-4368-92e2-93c561a65493"
      , "variante": "uso dermatológico"
      , "dosis máxima diaria": 42
      }

    , "metadata":
      { "timestamp": "2014-01-14T22:30:11,569292680-0430"
      , "revocation": "2014-01-14T22:31:21,965875745-0430"
      }

    , "links":
      [ { "rel": ["self"               ], "href": "http://api.pcaaudit.com/v1/database/variante/version/c4462cf1-33d8-4123-8a05-a2ee318ff141"                     }
      , { "rel": ["predecessor-version"], "href": "http://api.pcaaudit.com/v1/database/variante/version/c4462cf1-33d8-4123-8a05-a2ee318ff141/predecessor-version" }
      , { "rel": ["successor-version"  ], "href": "http://api.pcaaudit.com/v1/database/variante/version/c4462cf1-33d8-4123-8a05-a2ee318ff141/successor-version"   }
      , { "rel": ["version-history"    ], "href": "http://api.pcaaudit.com/v1/database/variante/version/c4462cf1-33d8-4123-8a05-a2ee318ff141/history"             }
      , { "rel": ["type"               ], "href": "http://api.pcaaudit.com/v1/database/variante"                                                                  }
      , { "rel": ["collection"         ], "href": "http://api.pcaaudit.com/v1/database/variante/rows"                                                             }

      , { "rel": ["http://api.pcaaudit.com/v1/database/variante/column/asociacion"]
        , "href": "http://api.pcaaudit.com/v1/database/asociacion/version/44bc82d6-b1da-4368-92e2-93c561a65493"
        }
      ]

    , "actions":
      [ { "name": "delete"
        , "title": "Delete"
        , "method": "DELETE"
        , "href": "http://api.pcaaudit.com/v1/database/variante/version/c4462cf1-33d8-4123-8a05-a2ee318ff141"
        }
      , { "name": "update"
        , "title": "Update"
        , "method": "PUT"
        , "href": "http://api.pcaaudit.com/v1/database/variante/version/c4462cf1-33d8-4123-8a05-a2ee318ff141"
        , "fields":
          [ { "name": "asociación farmacológica"
            , "type": "http://api.pcaaudit.com/v1/database/variante/column/asociacion"
            , "value": "http://api.pcaaudit.com/v1/database/asociacion/version/44bc82d6-b1da-4368-92e2-93c561a65493"
            }
          , { "name": "variante"
            , "type": "http://api.pcaaudit.com/v1/database/variante/column/variante"
            , "value": "uso dermatológico"
            }
          , { "name": "dosis máxima diaria"
            , "type": "http://api.pcaaudit.com/v1/database/variante/column/dmd"
            , "value": 42
            }
          ]
        }
      ]
    }



## TODO: Version history of some specific version.

This should be similar to a collection, but order becomes very
important.  Mind the possibility of key-changing updates — version
histories should be based on a recursive query that follows the full
history of a row, even across identities!



## TODO: Full version history browsing with filters.

This refers to version history for rows with no active version.
As they won’t appear in the table active row collection, their
history is thence unreachable.

This definitely sounds useful: consider PCA Audit savings reports
that have to look up deleted irregularity reports.  Without this, not
all of the data in the database would be accessible through the API,
which would be a shame.  Perhaps it’s best to simply provide access
to the full version history of a resource type and provide filters,
which would be closer to the representation in the Squealer schema.
