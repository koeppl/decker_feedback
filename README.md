# API V1

## Endpoints

### `GET /token`

Creates and returns a session token of the form:

``` {.json}
{
    "random": "1234567",
    "authorized": "abcdefg",  // if authenticated
    "admin": "ABCDEFG"        // if authorized as admin
}
```

### `POST /comments`

Creates a new comment from data in the form of:

``` {.json}
{
    "token": "1234567",
    "markdown": "The comment text.",
    "deck": "the-deck-id",
    "slide": "the-slide-id",
}
```

Or updates an existing comment from data in the form of:

``` {.json}
{
    "id": 123,
    "token": "1234567",
    "markdown": "The comment text.",
    "deck": "the-deck-id",
    "slide": "the-slide-id",
}
```

### `PUT /comments`

Retrieves the list of all comments for a particular slide identified by:

``` {.json}
{
    "token": "1234567",
    "deck": "the-deck-id",
    "slide": "the-slide-id"
}
```

The comment list looks like this:

```json
[
    {
        "id": 123,
        "author": "1234567",
        "markdown": "<p>The comment text.</p>",
        "html": "The comment text.",
        "slide": "the-slide-id",
        "created": "2020-12-31",
        "votes": 3,
        "didvote": true,
        "answers": []
    },
    {
        "id": 124,
        "author": "1234567",
        "markdown": "The comment text.",
        "html": "<p>The comment text.</p>",
        "slide": "the-slide-id",
        "created": "2020-12-31",
        "votes": 2,
        "didvote": false,
        "answers:" [
            {
                "id": 456,
                "created": "2020-12-31"
            },
            {
                "id": 457,
                "markdown": "The answer text.",
                "html": "<p>The answer text.</p>",
                "link": "http://answer.com/456",
                "created": "2020-12-31"
            }
        ]
    }
]
```

### `DELETE /comments`

Deletes the specified comment.

``` {.json}
{
    "id": 123,
    "token": "1234567"
}
```

### `PUT /login`

Authenticates an admin user from credentials in the form of:

``` {.json}
{
    "login": "login-name",
    "password": "Secret",
    "deck": "the-deck-id"
}
```

### `PUT /vote`

Toggle the vote on a comment (Not idempotent, BAD).

``` {.json}
{
    "comment": 123,     // Comment id
    "voter": "1234567"  // Voter token
}
```

### `POST /answers`

Create a new answer for a comment from data like this:

``` {.json}
{
    "token": "1234567",                         // admin token
    "comment": 123,
    "markdown": "Answer text",                  // optional
    "link": "https://answer.com/answer/to/123"  // optional
}
```

### `DELETE /answers`

Deletes the identified answer:

``` {.json}
{
    "id": 123,
    "token": "1234567"
}
```

## Javascript functions

Simple wrapper functions around the API endpoints using the Javascript
`fetch()` API.

``` {.javascript}
{
    getToken: async () => ...
    getLogin: async (credentials) => ...
    getComments: async (deck, slide, token) => ...
    submitComment: (deck, slide, token, markdown, id, answered) => ...
    deleteComment: (key, token) => ...
    voteComment: async (vote) => ...
    postAnswer: (commentId, token, markdown, link) => ...
    deleteAnswer: (id, token) => ...
}
```

Not very consistent. `getLogin` and `voteComment` take one object
parameter, all others take separate parameters.

# API V2

This will be more RESTish.

# Internal notes

## Servant Links

-   [Servant - haskell-servant](https://www.servant.dev)
-   [servant -- A Type-Level Web DSL --- Servant
    documentation](https://docs.servant.dev/en/stable/index.html)
-   [servant-persistent](https://www.parsonsmatt.org/2015/06/07/servant-persistent.html)
-   [Hackage:
    persistent-sqlite](https://hackage.haskell.org/package/persistent-sqlite)
-   [haskell-servant /
    example-servant-persistent](https://github.com/haskell-servant/example-servant-persistent)

## Authentication

|                    | public server | beuth ldap server |
|--------------------|---------------|-------------------|
| token              | random token  | auth token        |
| login (basic auth) | admin token   | ---               |
|                    |               |                   |
