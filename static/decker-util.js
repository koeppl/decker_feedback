export { buildApi };

function buildApi(url) {
  return {
    getToken: async () => {
      return fetch(url + "/token", {
        method: "GET",
        // mode: "cors",
        credentials: "include",
        cache: "no-store"
      }).then(response => response.json());
    },

    getLogin: async () => {
      return fetch(url + "/login", {
        method: "GET",
        // mode: "cors",
        credentials: "include",
        cache: "no-store"
      }).then(response => response.json());
    },

    getComments: async (deck, slide, token) => {
      let data = { deck: deck, slide: slide, token: token };
      return fetch(url + "/comments", {
        /* Need to use put, because server does not accept data in
         * request body of GET. */
        method: "PUT",
        cache: "no-store",
        body: JSON.stringify(data)
      }).then(response => response.json());
    },

    submitComment: (deck, slide, token, markdown) => {
      if (!markdown) return;
      let data = {
        deck: deck,
        slide: slide,
        token: token,
        markdown: markdown
      };
      return fetch(url + "/comments", {
        method: "POST",
        body: JSON.stringify(data)
      });
    },

    deleteComment: (key, token) => {
      let data = { key: key, token: token };
      return fetch(url + "/comments", {
        method: "DELETE",
        body: JSON.stringify(data)
      });
    }
  };
}
