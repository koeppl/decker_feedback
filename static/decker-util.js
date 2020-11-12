export {buildApi};

function buildApi(base) {
  let cors = window.location.origin !== new URL(base).origin;

  console.log("buildApi: cors = " + cors);

  return {
    getToken: async () => {
      return fetch(base + "/token", {
        method: "GET",
        mode: cors ? "cors" : "same-origin",
        credentials: cors ? "omit" : "include",
        cache: "no-store"
      }).then(response => response.json());
    },

    getLogin: async (credentials) => {
      return fetch(base + "/login", {
        method: "PUT",
        mode: cors ? "cors" : "same-origin",
        cache: "no-store",
        body: JSON.stringify(credentials)
      }).then(response => response.json());
    },

    getComments: async (deck, slide, token) => {
      let data = {deck: deck, slide: slide, token: token};
      return fetch(base + "/comments", {
        /* Need to use put, because server does not accept data in
         * request body of GET. */
        method: "PUT",
        mode: cors ? "cors" : "same-origin",
        cache: "no-store",
        body: JSON.stringify(data)
      }).then(response => response.json());
    },

    submitComment: (deck, slide, token, markdown, commentId) => {
      if (!markdown) return;
      let data = {
        deck: deck,
        slide: slide,
        token: token,
        markdown: markdown,
        id: commentId ? commentId : null
      };
      return fetch(base + "/comments", {
        method: "POST",
        mode: cors ? "cors" : "same-origin",
        body: JSON.stringify(data)
      });
    },

    deleteComment: (key, token) => {
      let data = {key: key, token: token};
      return fetch(base + "/comments", {
        method: "DELETE",
        mode: cors ? "cors" : "same-origin",
        body: JSON.stringify(data)
      });
    },

    voteComment: async (vote) => {
      return fetch(base + "/vote", {
        method: "PUT",
        mode: cors ? "cors" : "same-origin",
        cache: "no-store",
        body: JSON.stringify(vote)
      });
    }
  };
}
