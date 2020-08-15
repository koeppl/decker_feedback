//import { updateCommentList, submitComment } from "./decker-util.js";
import * as util from "./decker-util.js";

window.addEventListener("load", _ => {
  let deckid = document.getElementById("deckid");
  let slideid = document.getElementById("slideid");
  let token = document.getElementById("persontoken");

  let container = document.getElementById("comment-list-1");
  let update = document.getElementById("update-button");

  let textarea = document.getElementById("add-comment-1");
  let submit = document.getElementById("submit-button");

  let getContext = () => {
    return {
      deck: deckid.value,
      slide: slideid.value,
      token: token.value
    };
  };

  let renderDelete = () => {
    let context = getContext();
    util.updateCommentList(getContext, renderList);
  };

  let renderSubmit = () => {
    let context = getContext();
    textarea.value = "";
    util.updateCommentList(getContext, renderList);
  };

  let renderList = list => {
    let context = getContext();
    while (container.firstChild) {
      container.removeChild(container.lastChild);
    }
    for (let comment of list) {
      let div = document.createElement("div");
      div.textContent = comment.html;
      if (comment.delete) {
        let del = document.createElement("button");
        del.textContent = "✖";
        del.addEventListener("click", _ => {
          util.deleteComment(getContext, comment.delete, renderDelete);
        });
        div.appendChild(del);
      }
      container.appendChild(div);
    }
    container.scrollTop = 0;
  };

  deckid.addEventListener("keydown", e => {
    if (e.key === "Enter") {
      util.updateCommentList(getContext, renderList);
      document.activeElement.blur();
    }
  });

  slideid.addEventListener("keydown", e => {
    if (e.key === "Enter") {
      util.updateCommentList(getContext, renderList);
      document.activeElement.blur();
    }
  });

  token.addEventListener("keydown", e => {
    if (e.key === "Enter") {
      util.updateCommentList(getContext, renderList);
      document.activeElement.blur();
    }
  });

  update.addEventListener("click", _ => {
    util.updateCommentList(getContext, renderList);
    document.activeElement.blur();
  });

  textarea.addEventListener("keydown", e => {
    if (e.key === "Enter" && e.shiftKey) {
      util.submitComment(getContext, textarea.value, renderSubmit);
      document.activeElement.blur();
    }
  });

  submit.addEventListener("click", _ => {
    util.submitComment(getContext, textarea.value, renderSubmit);
    document.activeElement.blur();
  });

  util.updateCommentList(getContext, renderList);
});
