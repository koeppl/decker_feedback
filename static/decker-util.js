import * as api from "./decker.js";

function submitComment(getContext, text, render) {
  let context = getContext();

  if (!text) {
    return;
  }

  if (context.token) {
    api.postCommentsByDeckBySlideByAuthor(
      context.deck,
      context.slide,
      context.token,
      text,
      _ => {
        render();
      },
      err => {
        console.log(err);
      }
    );
  } else {
    api.postCommentsByDeckBySlide(
      context.deck,
      context.slide,
      text,
      _ => {
        render();
      },
      err => {
        console.log(err);
      }
    );
  }
}

function updateCommentList(getContext, render) {
  let context = getContext();
  if (context.token !== "") {
    api.getCommentsByDeckBySlideByAuthor(
      context.deck,
      context.slide,
      context.token,
      list => {
        render(list);
      },
      err => {
        console.log(err);
      }
    );
  } else {
    api.getCommentsByDeckBySlide(
      context.deck,
      context.slide,
      list => {
        render(list);
      },
      err => {
        console.log(err);
      }
    );
  }
}

function deleteComment(getContext, key, render) {
  let context = getContext();
  api.deleteCommentsByIdByToken(
    key,
    context.token,
    _ => {
      render();
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
      api.getAuthorsById(
        comment.author,
        author => {
          if (author.token === context.token) {
            let del = document.createElement("button");
            del.textContent = "✖";
            del.addEventListener("click", _ => {
              api.deleteCommentsByIdByToken(
                key,
                context.token,
                _ => {
                  updateCommentList(getContext);
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

export { updateCommentList, submitComment, deleteComment };
