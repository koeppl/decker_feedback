import { buildApi } from "./decker-util.js";

let util = buildApi(window.location.origin);
window.Decker = util;

window.addEventListener("load", async _ => {
  let deckid = document.getElementById("deckid");
  let slideid = document.getElementById("slideid");
  let token = document.getElementById("persontoken");

  let container = document.getElementById("comment-list-1");
  let update = document.getElementById("update-button");

  let textarea = document.getElementById("add-comment-1");
  let submit = document.getElementById("submit-button");

  var serverToken;

  try {
    serverToken = await util.getToken();
  } catch (err) {
    console.log("Can't fetch token from '.'. This is useless. Problem was:");
    console.log(err);
    return;
  }

  let getContext = () => {
    let url = new URL(window.location);
    url.hash = "";
    url.query = "";
    url.username = "";
    url.password = "";
    deckid.value = url.href;
    if (serverToken.authorized) {
      return {
        deck: url.href,
        slide: slideid.value,
        token: serverToken.authorized
      };
    } else {
      return { deck: url.href, slide: slideid.value, token: token.value };
    }
  };

  let updateToken = () => {
    if (serverToken.authorized) {
      token.setAttribute("disabled", "disabled");
      token.value = serverToken.authorized;
    } else {
      token.addEventListener("keydown", e => {
        if (e.key === "Enter") {
          updateComments();
          document.activeElement.blur();
        }
      });
      token.value = serverToken.random;
    }
  };

  let updateComments = () => {
    let context = getContext();
    util
      .getComments(context.deck, context.slide, context.token)
      .then(renderList)
      .catch(console.log);
  };

  let renderSubmit = () => {
    updateComments();
    textarea.value = "";
  };

  let renderList = list => {
    while (container.firstChild) {
      container.removeChild(container.lastChild);
    }
    for (let comment of list) {
      console.log(comment);
      let div = document.createElement("div");
      div.innerHTML = comment.html;
      if (comment.delete) {
        let del = document.createElement("button");
        del.textContent = "✖";
        del.addEventListener("click", _ => {
          let context = getContext();
          util
            .deleteComment(comment.delete, context.token)
            .then(updateComments);
        });
        div.appendChild(del);
      }
      container.appendChild(div);
    }
    container.scrollTop = 0;
  };

  deckid.addEventListener("keydown", e => {
    if (e.key === "Enter") {
      updateComments();
      document.activeElement.blur();
    }
  });

  slideid.addEventListener("keydown", e => {
    if (e.key === "Enter") {
      updateComments();
      document.activeElement.blur();
    }
  });

  update.addEventListener("click", _ => {
    updateComments();
    document.activeElement.blur();
  });

  textarea.addEventListener("keydown", e => {
    if (e.key === "Enter" && e.shiftKey) {
      let context = getContext();
      util
        .submitComment(
          context.deck,
          context.slide,
          context.token,
          textarea.value
        )
        .then(renderSubmit)
        .catch(console.log);
      document.activeElement.blur();
    }
  });

  submit.addEventListener("click", _ => {
    let context = getContext();
    util
      .submitComment(context.deck, context.slide, context.token, textarea.value)
      .then(renderSubmit)
      .catch(console.log);
    document.activeElement.blur();
  });

  updateToken();
  updateComments();
  document.activeElement.blur();
});
