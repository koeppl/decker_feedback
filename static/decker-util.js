import "./atomic.js";

export { buildApi };

function buildApi(url) {
  return {
    getToken: () => {
      return atomic(url + "/token", {
        responseType: "json",
        withCredentials: true
      }).then(r => r.data);
    },

    getComments: (deck, slide, token) => {
      let data = {
        deck: deck,
        slide: slide,
        token: token
      };
      console.log(data);
      return atomic(url + "/comments", {
        /* Need to use put, because server does not accept data in request body
         * of GET. */
        method: "PUT",
        responseType: "json",
        headers: {
          // The 't' in Content-type has to be lowercase for atomic to work!
          "Content-type": "application/json"
        },
        data: data
      }).then(r => r.data);
    },

    submitComment: (deck, slide, token, markdown) => {
      if (!markdown) return;
      let data = {
        deck: deck,
        slide: slide,
        token: token,
        markdown: markdown
      };
      console.log(data);
      return atomic(url + "/comments", {
        method: "POST",
        headers: {
          // The 't' in Content-type has to be lowercase for atomic to work!
          "Content-type": "application/json"
        },
        responseType: "json",
        data: data
      });
    },

    deleteComment: (key, token) => {
      let data = {
        key: key,
        token: token
      };
      console.log(data);
      return atomic(url + "/comments", {
        method: "DELETE",
        headers: {
          // The 't' in Content-type has to be lowercase for atomic to work!
          "Content-type": "application/json"
        },
        data: data
      });
    }
  };
}
