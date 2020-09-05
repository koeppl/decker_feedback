import "./atomic.js";

export {buildApi};

function buildApi(url) {
  return {
    getToken: () => {
      return atomic(url + "/token", {
        responseType: "json",
        withCredentials: true
      }).then(r => r.data);
    },

    getComments: (deck, slide, token) => {
      return atomic(url + `/comments/${deck}/${slide}/${token}`, {
        responseType: "json"
      }).then(r => r.data);
    },

    submitComment: (deck, slide, token, text) => {
      if (!text) return;

      let options = {
        method: "POST",
        headers: {
          // The 't' in Content-type has to be lowercase for atomic to work!
          "Content-type": "application/json"
        },
        data: { html: text }
      };

      if (token) {
        return atomic(url + `/comments/${deck}/${slide}/${token}`, options);
      } else {
        return atomic(url + `/comments/${deck}/${slide}`, options);
      }
    },

    deleteComment: (id, token) => {
      return atomic(url + `/comments/${id}/${token}`, {
        method: "DELETE"
      });
    }
  };
}
