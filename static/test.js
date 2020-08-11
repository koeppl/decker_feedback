console.log("You're on!");

window.addEventListener("load", e => {
  let deckid = document.getElementById("deckid");
  let slideid = document.getElementById("slideid");
  let token = document.getElementById("persontoken");
  let update = document.getElementById("update-button");
  let textarea = document.getElementById("add-comment-1");
  let submit = document.getElementById("submit-button");

  deckid.addEventListener("keydown", e => {
    if (e.key === "Enter") updateCommentList();
  });

  slideid.addEventListener("keydown", e => {
    if (e.key === "Enter") updateCommentList();
  });

  token.addEventListener("keydown", e => {
    if (e.key === "Enter") updateCommentList();
  });

  update.addEventListener("click", e => {
    updateCommentList();
  });

  textarea.addEventListener("keydown", e => {
    if (e.key === "Enter" && e.shiftKey) {
      submitComment();
      updateCommentList();
    }
  });

  submit.addEventListener("click", e => {
    submitComment();
    updateCommentList();
  });

  updateCommentList();
});

function submitComment() {
  let deckid = document.getElementById("deckid").value;
  let slideid = document.getElementById("slideid").value;
  let token = document.getElementById("persontoken").value;
  let textarea = document.getElementById("add-comment-1");

  if (!textarea.value) {
    return;
  }

  postCommentsByDeckBySlideByAuthor(
    deckid,
    slideid,
    token,
    textarea.value,
    list => {
      textarea.value = "";
      updateCommentList();
    },
    err => {
      console.log(err);
    }
  );
}

function updateCommentList() {
  let deckid = document.getElementById("deckid").value;
  let slideid = document.getElementById("slideid").value;
  let token = document.getElementById("persontoken").value;
  let submit = document.getElementById("submit-button");

  let container = document.getElementById("comment-list-1");

  getCommentsByDeckBySlide(
    deckid,
    slideid,
    list => {
      fillContainer(container, token, list);
    },
    err => {
      console.log(err);
    }
  );
}

function fillContainer(container, token, list) {
  while (container.firstChild) {
    container.removeChild(container.lastChild);
  }
  for (let [key, comment] of list) {
    let div = document.createElement("div");
    div.textContent = comment.markdown;
    getAuthorsById(
      comment.author,
      author => {
        if (author.token === token) {
          let del = document.createElement("button");
          del.textContent = "✖";
          del.addEventListener("click", e => {
            console.log(key);
            deleteCommentsById(
              key,
              s => {
                updateCommentList();
              },
              e => {
                console.log(e);
              }
            );
          });
          div.appendChild(del);
        }
      },
      err => {
        console.log(err);
      }
    );
    container.appendChild(div);
  }
  container.scrollTop = 0;
}
