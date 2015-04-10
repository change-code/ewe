function on_load() {
    function State(filename, created) {
        this.filename = filename;
        this.created = created;
        this.compiled = false;
        this.compile_log = "";
        this.session = null;

        this.node = document.createElement("li");
        var button_delete = document.createElement("button");
        button_delete.className = "delete";
        button_delete.appendChild(document.createTextNode("x"));
        var button_switch = document.createElement("button");
        this.text_node = document.createTextNode(filename);

        button_switch.addEventListener(
            "click",
            function() {
                switch_file(filename);
            });

        button_delete.addEventListener(
            "click",
            function() {
                delete_file(filename);
            });

        button_switch.appendChild(this.text_node);
        this.node.appendChild(button_delete);
        this.node.appendChild(button_switch);
    }

    State.prototype.create_session = function(content) {
        this.session = ace.createEditSession(content, "ace/mode/erlang");
        this.session.setOption("useWorker", false);
        this.session.setUseSoftTabs(true);
    };

    State.prototype.is_saved = function() {
        if (!this.created)
            return false;
        if (this.session === null)
            return true;
        return !this.session.getUndoManager().hasUndo();
    };


    var socket;
    var term;

    var button_create_file = document.getElementById("create-file");
    var button_refresh_filelist = document.getElementById("refresh-filelist");

    var button_save_file = document.getElementById("save-file");
    var button_compile_file = document.getElementById("compile-file");
    var button_load_file = document.getElementById("load-file");

    var button_toggle_error_log = document.getElementById("toggle-error-log");
    var button_toggle_erlang_shell = document.getElementById("toggle-erlang-shell");
    var button_toggle_compile_log = document.getElementById("toggle-compile-log");
    var container_error_log = document.getElementById("error-log-container");
    var container_erlang_shell = document.getElementById("erlang-shell-container");
    var container_compile_log = document.getElementById("compile-log-container");

    var tbody_error_log = document.getElementById("error-log");
    var div_erlang_shell = document.getElementById("erlang-shell");

    var container_filelist = document.getElementById("filelist");

    var file_states = new Map();
    var current_filename = null;
    var current_tab = null;
    var tabs =
        new Map(
            [["error-log", [button_toggle_error_log, container_error_log]],
             ["erlang-shell", [button_toggle_erlang_shell, container_erlang_shell]],
             ["compile-log", [button_toggle_compile_log, container_compile_log]]]
        );

    function LOG(type, desc) {
        var tr = document.createElement("tr");
        var td_type = document.createElement("td");
        td_type.appendChild(
            document.createTextNode(type.toUpperCase()));
        var td_time = document.createElement("td");
        td_time.appendChild(
            document.createTextNode(new Date().toLocaleString()));
        var td_desc = document.createElement("td");
        td_desc.appendChild(
            document.createTextNode(desc));
        tr.appendChild(td_type);
        tr.appendChild(td_time);
        tr.appendChild(td_desc);
        tbody_error_log.appendChild(tr);

        if (current_tab !== "error-log") {
            toggle_tab("error-log");
        }

        tr.scrollIntoView();
    }

    function http_file_req(method, path, data) {
        var req = new XMLHttpRequest();
        req.open(method, "/file/"+path, false);
        req.send(data);
        return req;
    }

    function reset_button() {
        if (current_filename === null) {
            button_save_file.disabled = "";
            button_compile_file.disabled = "disabled";
            button_load_file.disabled = "disabled";
            return;
        }

        var state = file_states.get(current_filename);
        if (state.is_saved()) {
            state.text_node.data = current_filename;
            button_save_file.disabled = "disabled";
            button_compile_file.disabled = "";
            if (state.compiled) {
                button_load_file.disabled = "";
            } else {
                button_load_file.disabled = "disabled";
            }
        } else {
            state.text_node.data = current_filename + " *";
            button_save_file.disabled = "";
            button_compile_file.disabled = "disabled";
            button_load_file.disabled = "disabled";
        }
    }


    function set_compile_log(log) {
        var pre_compile_log = document.getElementById("compile-log");
        var pre_new = document.createElement("pre");
        pre_new.id = "compile-log";
        pre_new.appendChild(document.createTextNode(log));
        pre_compile_log.parentNode.replaceChild(pre_new, pre_compile_log);

        if (log !== "") {
            if (current_tab !== 'compile-log') {
                toggle_tab("compile-log");
            }
        } else {
            if (current_tab === 'compile-log') {
                toggle_tab("compile-log");
            }
        }
    }

    function filelist_insert(state) {
        var filename = state.filename;
        var filenames = [name for (name of file_states.keys())];
        filenames.sort();

        var found = null;

        for (var name of filenames) {
            if (name > filename) {
                found = name;
                break;
            }
        }

        if (found === null) {
            container_filelist.appendChild(state.node);
        } else {
            container_filelist.insertBefore(
                state.node,
                file_states.get(found).node);
        }
    }


    function create_file() {
        var result = window.prompt("Please input module name:");
        if (result === null) {
            return;
        }

        if (result === "") {
            window.alert("module name must not be empty");
            return;
        }

        var filename = result + ".erl"
        if (file_states.has(filename)) {
            return;
        }

        var state = new State(filename, false);
        state.text_node.data += " *";
        filelist_insert(state);
        file_states.set(filename, state);
        switch_file(filename);
    }


    function refresh_filelist() {
        button_create_file.disabled = "disabled";
        button_refresh_filelist.disabled = "disabled";
        button_save_file.disabled = "disabled";
        button_compile_file.disabled = "disabled";
        button_load_file.disabled = "disabled";

        var req = http_file_req("GET", "", null);
        if (req.status === 200) {
            var new_files = JSON.parse(req.response);
            var new_states = new Map();

            for (let state of file_states.values()) {
                if (state.is_saved())
                    continue;
                new_states.set(state.filename, state);
            }

            for (let name of new_files) {
                if (new_states.has(name))
                    continue;
                new_states.set(name, new State(name, true));
            }

            let filenames = [name for (name of new_states.keys())];
            filenames.sort();

            var new_container = document.createElement("ul");
            new_container.id = "filelist";

            for (let filename of filenames) {
                let state = new_states.get(filename);
                if (filename === current_filename) {
                    state.node.className = "selected";
                }
                new_container.appendChild(state.node);
            }

            container_filelist.parentNode.replaceChild(new_container, container_filelist);
            container_filelist = new_container;

            if (!new_states.has(current_filename)) {
                current_filename = null;
            }

            file_states = new_states;
        } else {
            LOG("FAILURE", "failed to refresh file list.");
        }

        reset_button();
        button_create_file.disabled = "";
        button_refresh_filelist.disabled = "";
    }


    function switch_file(filename) {
        if (current_filename === null) {
            if (editor.getSession().getUndoManager().hasUndo()) {
                if (!window.confirm("current session is not saved, do you really want to edit '" + filename + "'?")) {
                    return;
                }
            }
        }

        if (current_filename !== null) {
            var current_state = file_states.get(current_filename);
            current_state.node.className = "";
        }

        current_filename = filename;
        var next_state = file_states.get(filename);
        next_state.node.className = "selected";

        if (next_state.session !== null) {
            reset_button();
            editor.setSession(next_state.session);
            set_compile_log(next_state.compile_log);
            return;
        }

        button_refresh_filelist.disabled = "disabled";
        button_save_file.disabled = "disabled";
        button_compile_file.disabled = "disabled";
        button_load_file.disabled = "disabled";
        editor.setReadOnly(true);
        set_compile_log("");

        var req = http_file_req("GET", filename, null);
        if (req.status === 200) {
            next_state.create_session(req.response);
        } else {
            LOG("FAILURE", "failed to load file '"+filename+"'");
            next_state.create_session("");
        }
        editor.setSession(next_state.session);

        button_refresh_filelist.disabled = "";
        reset_button();
        editor.setReadOnly(false);
        editor.focus();
    }


    function delete_file(filename) {
        if (filename === current_filename)
            return;

        if (!window.confirm("Do you really want to delete '"+filename+"'?")) {
            return;
        }

        var state = file_states.get(filename);

        if (state.created) {
            var req = http_file_req("DELETE", filename, null);
            if (req.status !== 200) {
                LOG("FAILURE", "failed to delete file '" + filename + "'");
                return;
            }
        }

        file_states.delete(filename);
        state.node.parentNode.removeChild(state.node);
    }


    function save_file() {
        if (current_filename !== null) {
            var state = file_states.get(current_filename);
            if (state.is_saved()) {
                return;
            }

            var text = state.session.getValue();
            var req = http_file_req("PUT", current_filename, text);

            if (req.status === 200) {
                state.compiled = false;
                state.session.getUndoManager().reset();
                reset_button();
            } else {
                LOG("FAILURE", "failed to save file '" + current_filename + "'");
            }
            editor.focus();
            return;
        }

        var result = window.prompt("Please input module name:");
        if (result === null) {
            return;
        }

        if (result === "") {
            window.alert("module name must not be empty");
            return;
        }

        var filename = result + ".erl"
        if (file_states.has(filename)) {
            window.alert("file '" + filename +"' already exists");
            return;
        }

        var state = new State(filename, false);
        state.node.className = "selected";
        state.text_node.data += " *";
        state.session = editor.getSession();

        filelist_insert(state);
        file_states.set(filename, state);
        current_filename = filename;
        save_file();
    }


    function compile_file() {
        if (current_filename === null)
            return;

        var state = file_states.get(current_filename);

        if (!state.is_saved())
            return;

        var req = http_file_req("COMPILE", current_filename, null);

        if (req.status === 200) {
            state.compiled = true;
        } else if (req.status === 400) {
            LOG("ERROR", "compilation error '"+current_filename+"'");
        } else {
            LOG("FAILURE", "failed to compile '"+current_filename+"'");
            return;
        }

        state.compile_log = req.response;
        var lines = req.response.split("\n");
        var annotations = [];

        for (let line of lines) {
            let parts = line.split(":");
            if (parts.length < 4)
                continue;

            let filename = parts.shift();
            let lno = parts.shift();
            let type = parts.shift().trim().toLowerCase();
            let error = parts.join(":").trimLeft();

            if (filename !== current_filename)
                continue;

            annotations.push(
                {row: lno - 1,
                 column: 0,
                 text: error,
                 type: type}
            );
        }

        state.session.setAnnotations(annotations);
        reset_button();
        set_compile_log(state.compile_log);
    }


    function load_file() {
        if (current_filename === null)
            return;

        var state = file_states.get(current_filename);

        if (!state.is_saved())
            return;

        if (!state.compiled)
            return;

        var req = http_file_req("LOAD", current_filename, null);
        if (req.status === 200) {
            if (current_tab !== "erlang-shell") {
                toggle_tab("erlang-shell");
            }
        } else if (req.status === 400) {
            LOG("ERROR", "failed to load '" + current_filename + "': " + req.response);
        } else {
            LOG("FAILURE", "failed to load '" + current_filename + "'");
        }
    }


    function toggle_tab(tab) {
        if (current_tab !== null) {
            let button_current = tabs.get(current_tab)[0];
            let container_current = tabs.get(current_tab)[1];
            button_current.parentNode.className = "";
            container_current.className = "tab";
        }

        if (current_tab === tab) {
            current_tab = null;
        } else {
            current_tab = tab;
            let button_next = tabs.get(current_tab)[0];
            let container_next = tabs.get(current_tab)[1];
            button_next.parentNode.className = "selected";
            container_next.className = "tab active";

            if (current_tab === "erlang-shell") {
                term.element.focus();
            }

        }
    }

    window.addEventListener(
        'beforeunload',
        function(event) {
            event = event || window.event;

            if (editor.getSession().getUndoManager().hasUndo()) {
                event.returnValue = "Do you really want to quit?";
            }

            for(let state of file_states.values()) {
                if (!state.is_saved()) {
                    event.returnValue = "Do you really want to quit?";
                }
            }

            return event.returnValue;
        });

    button_create_file.addEventListener("click", create_file);
    button_refresh_filelist.addEventListener("click", refresh_filelist);

    button_save_file.addEventListener("click", save_file);
    button_compile_file.addEventListener("click", compile_file);
    button_load_file.addEventListener("click", load_file);

    button_toggle_error_log.addEventListener(
        "click",
        function () {
            toggle_tab("error-log");
        });
    button_toggle_erlang_shell.addEventListener(
        "click",
        function () {
            toggle_tab("erlang-shell");
        });
    button_toggle_compile_log.addEventListener(
        "click",
        function () {
            toggle_tab("compile-log");
        });

    refresh_filelist();

    var editor = ace.edit("editor");
    editor.getSession().setMode("ace/mode/erlang");
    editor.getSession().setOption("useWorker", false);
    editor.getSession().setUseSoftTabs(true);

    editor.on(
        "change",
        function(event) {
            reset_button();
        });

    editor.focus();

    socket = new WebSocket("ws://" + location.host + "/");
    socket.onopen =
        function () {
            term = new Terminal(
                {cols: 80,
                 rows: 512
                });

            term.open(div_erlang_shell);

            term.on(
                'data',
                function (data) {
                    socket.send(data);
                });

            socket.onmessage =
                function (message) {
                    term.write(message.data);
                    term.children[term.y].scrollIntoView(false);
                };

            socket.onclose =
                function () {
                    term.destroy();
                };
        };
}


window.addEventListener('load', on_load);
