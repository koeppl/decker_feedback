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

  deckid.addEventListener("keydown", e => {
    if (e.key === "Enter") {
      util.updateCommentList(getContext, container);
      document.activeElement.blur();
    }
  });

  slideid.addEventListener("keydown", e => {
    if (e.key === "Enter") {
      util.updateCommentList(getContext, container);
      document.activeElement.blur();
    }
  });

  token.addEventListener("keydown", e => {
    if (e.key === "Enter") {
      util.updateCommentList(getContext, container);
      document.activeElement.blur();
    }
  });

  update.addEventListener("click", _ => {
    util.updateCommentList(getContext, container);
  });

  textarea.addEventListener("keydown", e => {
    if (e.key === "Enter" && e.shiftKey) {
      util.submitComment(getContext, container, textarea);
      document.activeElement.blur();
    }
  });

  submit.addEventListener("click", _ => {
    util.submitComment(getContext, container, textarea);
  });

  util.updateCommentList(getContext, container);
});
