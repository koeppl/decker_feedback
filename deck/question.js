Reveal.addEventListener("ready", event => {
  console.log(event.currentSlide.id);

  let body = document.querySelector("body");

  let panel = document.createElement("div");
  let header = document.createElement("div");
  let user = document.createElement("input");
  let close = document.createElement("div");
  let list = document.createElement("div");
  let input = document.createElement("div");
  let text = document.createElement("textarea");
  let footer = document.createElement("div");

  panel.classList.add("q-panel");
  header.classList.add("q-header");
  header.appendChild(user);
  header.appendChild(close);
  close.classList.add("q-close");
  close.textContent = "✖";

  list.classList.add("q-list");

  input.classList.add("q-input");
  input.appendChild(text);
  text.setAttribute("rows", 4);

  footer.classList.add("q-footer");
  footer.textContent = "Footer";

  panel.appendChild(header);
  panel.appendChild(list);
  panel.appendChild(input);
  panel.appendChild(footer);

  document.body.appendChild(panel);

  let getContext = () => {
    return {
      deckid: body.getAttribute("data-deckid"),
      slideid: Reveal.getCurrentSlide().id,
      token: user.value
    };
  };

  user.addEventListener("keydown", e => {
    if (e.key === "Enter") {
      updateCommentList(getContext, list);
      e.stopPropagation();
    }
  });

  text.addEventListener("keydown", e => {
    if (e.key === "Enter" && e.shiftKey) {
      submitComment(getContext, list, e.target);
      e.stopPropagation();
      e.preventDefault();
    }
  });
});
