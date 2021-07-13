export { startPanel };

const deckId = "http://localhost:8888/test/decks/engine-local-deck.html";
const slideId = "title-slide";

import { h, html, Component, render } from "./htm-preact.js";

import { buildApi } from "http://localhost:8081/decker-util.js";

let debugToken = "ddfd674fa";

class Comment extends Component {
  state = { editing: false };

  submit = (ev) => {
    ev.preventDefault();
    this.setState({ editing: false });
    let { app, comment } = this.props;
    console.log("submit", comment.id, this.markdown);
    app.updateComment(comment.id, this.markdown);
  };

  cancel = (ev) => {
    ev.preventDefault();
    this.setState({ editing: false });
  };

  edit = (ev) => {
    ev.preventDefault();
    this.setState({ editing: true });
  };

  delete = (ev) => {
    ev.preventDefault();
    let { app, comment } = this.props;
    app.deleteComment(comment.id);
  };

  input = (ev) => {
    this.markdown = ev.target.value;
  };

  render(props, state) {
    let { app, comment } = props;
    let canDelete = comment.author == app.state.token.random;

    if (this.state.editing || comment.brandNew) {
      return html`
        <div class="comment">
          <textarea rows="5" cols="40" onInput=${this.input}>
            ${raw(comment.markdown)}
          </textarea
          >
          <button type="button" onClick=${this.submit}>Update</button>
          ${!comment.brandNew
            ? html`<button type="button" onClick=${this.cancel}>Cancel</button>`
            : html``}
        </div>
      `;
    } else {
      return html`
        <div class="comment">
          <div class="author">
            <small>${comment.id}, ${comment.author}</small>
          </div>
          ${raw(comment.html)}
          ${canDelete
            ? html` <button type="button" onClick=${this.edit}>Edit</button>
                <button type="button" onClick=${this.delete}>Delete</button>`
            : html``}
        </div>
      `;
    }
  }
}

class Answer extends Component {
  state = { editing: false };

  render(props) {
    let { answer } = props;
    if (answer.link || answer.html) {
      return html`<div class="answer">${raw(answer.html)}</div>`;
    } else {
      return html``;
    }
  }
}

class Header extends Component {
  state = {};

  render(props) {
    return html`<div class="header">Here be buttons and stuff.</div>`;
  }
}

class Footer extends Component {
  state = {};

  render(props) {
    let { app } = props;
    return html`<div class="footer">
      <p>Here be buttons and more stuff.</p>
      <p>
        <i><small>(${app.state.deckId}#${app.state.slideId})</small></i>
      </p>
    </div>`;
  }
}

let engine = buildApi("http://localhost:8081");

class App extends Component {
  constructor() {
    super();
    this.state = {
      deckId: deckId,
      slideId: slideId,
      token: null,
      comments: [],
    };
    this.getToken().then(this.refreshComments);
  }

  getToken = async () => {
    let token = await engine.getToken();
    token.random = debugToken;
    this.setState({ token });
  };

  getComments = async () => {
    let { deckId, slideId, token } = this.state;
    return engine.getComments(deckId, slideId, token.random);
  };

  refreshComments = async () => {
    this.getComments().then((comments) => {
      this.setState({ comments });
    });
  };

  updateComment = async (id, markdown) => {
    let { deckId, slideId, token } = this.state;
    engine
      .submitComment(deckId, slideId, token.random, markdown, id)
      .then((_) => {
        this.refreshComments();
      });
  };

  deleteComment = async (id) => {
    let { token } = this.state;
    engine.deleteComment(id, token.random).then((_) => {
      this.refreshComments();
    });
  };

  newComment = async () => {
    let { deckId, slideId, token } = this.state;
    let id = await engine.submitComment(
      deckId,
      slideId,
      token.random,
      "",
      null
    );
    console.log("new comment", id);
    let comments = await this.getComments();
    let comment = comments.find((c) => {
      return c.id == id;
    });
    if (comment) {
      comment.brandNew = true;
    }
    this.setState({ comments });
  };

  makeEditable = (id) => {
    let comment = this.state.comments.find((c) => {
      return c.id == id;
    });
    console.log("make editable", id, comment, this.state.comments);
    if (comment) {
      comment.setState({ editing: true });
    }
  };

  render(props, state) {
    return html`
      <div class="panel">
        <${Header} app=${this} />
        <div class="comments">
          ${state.comments.map(
            (comment) => html`
              <${Comment} app=${this} comment="${comment}" />
              ${comment.answers.map(
                (answer) => html`<${Answer} app=${this} answer="${answer}" />`
              )}
            `
          )}
          <div class="add-comment">
            <button onClick=${this.newComment}>+</button>
          </div>
        </div>
        <${Footer} app=${this} />
      </div>
    `;
  }
}

async function startPanel(selector) {
  render(html`<${App} />`, document.getElementById(selector));
}

// Parses a string containing raw html so it can be spliced into recursive
// html`` templates.
function raw(str) {
  return html([str], []);
}
