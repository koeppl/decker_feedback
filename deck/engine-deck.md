---
controls: 0
deck-id: 'decker-engine-test'
template:
  css: question.css
  include-js:
  - question.js
  - /api/decker.js
  - '/api/decker-util.js'
title: Decker Engine Test
---

# Decker Engine

## How stupid can a name be?

-   Amazing what lack of imagination can do to a perfectly fine webapp

## Any other questions?

-   Just type them into the text area and submit them with ⇧⏎
    (Shift-Return)
-   If you want to be able to delete a question later, provide a user
    token

# TODO (1)

## Frontend

-   [ ] Slide the question pane in and out of view
    -   [ ] Add question mark to slide
    -   [x] Add close button to pane
    -   [ ] Animate sliding
-   [ ] Remove the background colors
-   [ ] Find just the right level of transparency
-   [ ] Offer to generate the user token from HTML basic authentication
    headers if available
-   [ ] Offer to store user token in a cookie
-   [ ] Provide an API to an authenticated lecturer
    -   [ ] Generate overviews (like *all questions for a deck sorted by
        slide*)
    -   [ ] Allow to delete any question

# TODO (2)

## Backend

-   [ ] Improve the API
    -   [ ] Remove internal ids and tokens from the endpoint URLs
-   [ ] Battle test the server
    -   [ ] Large videos
    -   [ ] Hundreds of simultaneous connections
    -   [ ] Malicious attacks

# Decker Engine Backstage Area

## Test page

-   [api/test.html](/api/test.html)

## API Doc

-   [api/docs-page.html](/public/api/docs-page.html)
