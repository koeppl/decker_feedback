
export var getComments = function(onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('GET', 'http://localhost:8081/comments', true);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onError(res);
      }
    }
  };
  xhr.send(null);
};

export var getCommentsByDeck = function(deck, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('GET', 'http://localhost:8081/comments/' + encodeURIComponent(deck) + '', true);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onError(res);
      }
    }
  };
  xhr.send(null);
};

export var getCommentsByDeckBySlide = function(deck, slide, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('GET', 'http://localhost:8081/comments/' + encodeURIComponent(deck) + '/' + encodeURIComponent(slide) + '', true);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onError(res);
      }
    }
  };
  xhr.send(null);
};

export var getCommentsByDeckBySlideByAuthor = function(deck, slide, author, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('GET', 'http://localhost:8081/comments/' + encodeURIComponent(deck) + '/' + encodeURIComponent(slide) + '/' + encodeURIComponent(author) + '', true);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onError(res);
      }
    }
  };
  xhr.send(null);
};

export var postCommentsByDeckBySlide = function(deck, slide, body, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('POST', 'http://localhost:8081/comments/' + encodeURIComponent(deck) + '/' + encodeURIComponent(slide) + '', true);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.setRequestHeader('Content-Type', 'application/json');
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onError(res);
      }
    }
  };
  xhr.send(JSON.stringify(body));
};

export var postCommentsByDeckBySlideByAuthor = function(deck, slide, author, body, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('POST', 'http://localhost:8081/comments/' + encodeURIComponent(deck) + '/' + encodeURIComponent(slide) + '/' + encodeURIComponent(author) + '', true);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.setRequestHeader('Content-Type', 'application/json');
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onError(res);
      }
    }
  };
  xhr.send(JSON.stringify(body));
};

export var deleteCommentsById = function(id, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('DELETE', 'http://localhost:8081/comments/' + encodeURIComponent(id) + '', true);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onError(res);
      }
    }
  };
  xhr.send(null);
};

export var getAuthors = function(onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('GET', 'http://localhost:8081/authors', true);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onError(res);
      }
    }
  };
  xhr.send(null);
};

export var getAuthorsById = function(id, onSuccess, onError) {
  var xhr = new XMLHttpRequest();
  xhr.open('GET', 'http://localhost:8081/authors/' + encodeURIComponent(id) + '', true);
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.onreadystatechange = function () {
    var res = null;
    if (xhr.readyState === 4) {
      if (xhr.status === 204 || xhr.status === 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onSuccess(res);
      } else {
        try { res = JSON.parse(xhr.responseText); } catch (e) { onError(e); }
        if (res) onError(res);
      }
    }
  };
  xhr.send(null);
};
