import * as api from "./decker.js";

export {
  getToken,
  getComments,
  updateCommentList,
  submitComment,
  deleteComment
};

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

function getComments(deck, slide, token) {
  return new Promise(function(resolve, reject) {
    api.getCommentsByDeckBySlideByAuthor(deck, slide, token, resolve, err => {
      reject({
        status: undefined,
        statusText: err
      });
    });
  });
}

function getToken() {
  let token = new Promise(function(resolve, reject) {
    getTokenInternal(resolve, err => {
      reject({
        status: undefined,
        statusText: err
      });
    });
  });
  return token;
}

function getTokenInternal(onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open("GET", "http://localhost:8081/token", true);
  // xhr.setRequestHeader("Authorization", headerAuthorization);
  xhr.withCredentials = true;
  xhr.setRequestHeader("Accept", "application/json");
  xhr.onreadystatechange = function() {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try {
          res = JSON.parse(xhr.responseText);
        } catch (e) {
          onError(e);
        }
        if (res) onSuccess(res);
      } else {
        try {
          res = JSON.parse(xhr.responseText);
        } catch (e) {
          onError(e);
        }
        if (res) onError(res);
      }
    }
  };
  xhr.send(null);
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
