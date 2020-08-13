function submitComment(getContext, container, textarea) {
  let context = getContext();

  if (!textarea.value) {
    return;
  }

  if (context.token) {
    postCommentsByDeckBySlideByAuthor(
      context.deck,
      context.slide,
      context.token,
      textarea.value,
      _ => {
        textarea.value = "";
        updateCommentList(getContext, container);
      },
      err => {
        console.log(err);
      }
    );
  } else {
    postCommentsByDeckBySlide(
      context.deck,
      context.slide,
      textarea.value,
      _ => {
        textarea.value = "";
        updateCommentList(getContext, container);
      },
      err => {
        console.log(err);
      }
    );
  }
}

function updateCommentList(getContext, container) {
  let context = getContext();
  getCommentsByDeckBySlide(
    context.deck,
    context.slide,
    list => {
      fillContainer(getContext, container, list);
    },
    err => {
      console.log(err);
    }
  );
}

function fillContainer(getContext, container, list) {
  let context = getContext();
  while (container.firstChild) {
    container.removeChild(container.lastChild);
  }
  for (let [key, comment] of list) {
    let div = document.createElement("div");
    div.textContent = comment.markdown;
    if (comment.author) {
      getAuthorsById(
        comment.author,
        author => {
          if (author.token === context.token) {
            let del = document.createElement("button");
            del.textContent = "✖";
            del.addEventListener("click", _ => {
              deleteCommentsById(
                key,
                _ => {
                  updateCommentList(getContext, container);
                },
                err => {
                  console.log(err);
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
    }
    container.appendChild(div);
  }
  container.scrollTop = 0;
}
