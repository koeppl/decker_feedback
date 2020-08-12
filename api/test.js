window.addEventListener("load", e => {
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
    if (e.key === "Enter") updateCommentList(getContext, container);
  });

  slideid.addEventListener("keydown", e => {
    if (e.key === "Enter") updateCommentList(getContext, container);
  });

  token.addEventListener("keydown", e => {
    if (e.key === "Enter") updateCommentList(getContext, container);
  });

  update.addEventListener("click", _ => {
    updateCommentList(getContext, container);
  });

  textarea.addEventListener("keydown", e => {
    if (e.key === "Enter" && e.shiftKey) {
      submitComment(getContext, container, textarea);
    }
  });

  submit.addEventListener("click", _ => {
    submitComment(getContext, container, textarea);
  });

  updateCommentList(getContext, container);
});

