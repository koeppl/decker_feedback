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

    getComments: async (query) => {
      return fetch(base + "/comments", {
        /* Need to use put, because server does not accept data in
         * request body of GET. */
        method: "PUT",
        mode: cors ? "cors" : "same-origin",
        cache: "no-store",
        body: JSON.stringify(query)
      }).then(response => response.json());
    },

    submitComment: (comment) => {
      return fetch(base + "/comments", {
        method: "POST",
        mode: cors ? "cors" : "same-origin",
        body: JSON.stringify(comment)
      });
    },

    deleteComment: (ident) => {
      return fetch(base + "/comments", {
        method: "DELETE",
        mode: cors ? "cors" : "same-origin",
        body: JSON.stringify(ident)
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
