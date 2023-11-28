#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <functional>

using std::cout;
using std::endl;
using std::map;
using std::string;
using std::vector;

namespace inlnk {

static string infSymbol = "∞";
static string fnSymbol = "⒡";

enum ilAtomTypes {
    INT = 1,
    FLOAT,
    BOOL,
    STRING,
    SYMBOL,
    COMMENT,
    DEF_WORD,
    STORE_SYMBOL,
    DELETE_SYMBOL,
    FUNC,
    IFUNC,
    FLOW_CONTROL,
    ERROR,
};

class IlAtom {
  public:
    ilAtomTypes t;
    int vi;
    double vf;
    string vs;
    bool vb;
    string name;
    std::function<void(vector<IlAtom> *)> vif;
    int jump_address;

    IlAtom() {
        t = ERROR;
        vs = "Not-Init";
    }

    string str() {
        switch (t) {
        case INT:
        case FLOAT:
        case BOOL:
            return vs;
            break;
        case STRING:
            return '"' + vs + '"';
            break;
        case IFUNC:
        case FUNC:
        case SYMBOL:
        case STORE_SYMBOL:
        case DELETE_SYMBOL:
        case FLOW_CONTROL:
        case DEF_WORD:
        case COMMENT:
            return vs;
            break;
        case ERROR:
            return "\n [Error: " + vs + "] ";
            break;
        }
        return "[UNEXPECTED TYPE]";
    }
};

class IndraLink {
  public:
    vector<IlAtom> stack;
    map<string, IlAtom> symbols;
    map<string, vector<IlAtom>> funcs;
    map<string, std::function<void(vector<IlAtom> *)>> inbuilts;
    vector<string> flow_control_words, def_words;

    void math_2ops(vector<IlAtom> *pst, string ops2) {
        size_t l = pst->size();
        if (l < 2) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Math-" + ops2 + "-Not-Enough-Operands";
            pst->push_back(err);
            return;
        }
        ilAtomTypes t1, t2;
        IlAtom res, op1, op2;
        op2 = pst->back();
        pst->pop_back();
        op1 = pst->back();
        pst->pop_back();
        t2 = op2.t;
        t1 = op1.t;
        if ((t1 != INT && t1 != FLOAT) || (t2 != INT && t2 != FLOAT)) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Math-" + ops2 + "-Wrong-Type-Operands";
            pst->push_back(err);
            return;
        }

        if (t1 == INT && t2 == INT) {
            int o1, o2;
            o1 = op1.vi;
            o2 = op2.vi;
            res.t = INT;
            if (ops2 == "+")
                res.vi = o1 + o2;
            else if (ops2 == "-")
                res.vi = o1 - o2;
            else if (ops2 == "*")
                res.vi = o1 * o2;
            else if (ops2 == "/") {
                if (o2 == 0) {
                    IlAtom err;
                    err.t = ERROR;
                    err.vs = "/-by-Zero";
                    pst->push_back(err);
                    return;
                }
                res.vi = o1 / o2;
            } else if (ops2 == "%") {
                if (o2 == 0) {
                    IlAtom err;
                    err.t = ERROR;
                    err.vs = "/-by-Zero";
                    pst->push_back(err);
                    return;
                }
                res.vi = o1 % o2;
            } else {
                IlAtom err;
                err.t = ERROR;
                err.vs = "Math-" + ops2 + "Unknown op-code";
                pst->push_back(err);
                return;
            }
            res.vs = std::to_string(res.vi);
        } else {
            double o1, o2;
            if (t1 == INT)
                o1 = (double)op1.vi;
            else
                o1 = op1.vf;
            if (t2 == INT)
                o2 = (double)op2.vi;
            else
                o2 = op2.vf;
            res.t = FLOAT;
            if (ops2 == "+")
                res.vf = o1 + o2;
            else if (ops2 == "-")
                res.vf = o1 - o2;
            else if (ops2 == "*")
                res.vf = o1 * o2;
            else if (ops2 == "/") {
                if (o2 == 0.0) {
                    IlAtom err;
                    err.t = ERROR;
                    err.vs = "/-by-Zero";
                    pst->push_back(err);
                    return;
                }
                res.vf = o1 / o2;
            } else {
                IlAtom err;
                err.t = ERROR;
                err.vs = "Unknown math-2ops: " + ops2;
                pst->push_back(err);
                return;
            }
            res.vs = std::to_string(res.vf);
        }
        pst->push_back(res);
    }

    void cmp_2ops(vector<IlAtom> *pst, string ops2) {
        size_t l = pst->size();
        if (l < 2) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "CMP-Not-Enough-Operands";
            pst->push_back(err);
            return;
        }
        ilAtomTypes t1, t2;
        IlAtom res, op1, op2;
        op2 = pst->back();
        pst->pop_back();
        op1 = pst->back();
        pst->pop_back();
        t2 = op2.t;
        t1 = op1.t;

        if ((t1 != INT && t1 != FLOAT) || (t2 != INT && t2 != FLOAT)) {
            if (t1 == BOOL && t2 == BOOL) {
                res.t = BOOL;
                if (ops2 == "==") {
                    res.vb = (op1.vb == op2.vb);
                } else if (ops2 == "!=") {
                    res.vb = (op1.vb != op2.vb);
                } else {
                    IlAtom err;
                    err.t = ERROR;
                    err.vs = "BOOL-Cmp-" + ops2 + "-Wrong-Type-Operands";
                    pst->push_back(err);
                    return;
                }
            } else if (t1 == STRING && t2 == STRING) {
                res.t = BOOL;
                if (ops2 == "==") {
                    res.vb = (op1.vs == op2.vs);
                } else if (ops2 == "!=") {
                    res.vb = (op1.vs != op2.vs);
                } else if (ops2 == ">=") {
                    res.vb = (op1.vs >= op2.vs);
                } else if (ops2 == "<=") {
                    res.vb = (op1.vs <= op2.vs);
                } else if (ops2 == "<") {
                    res.vb = (op1.vs < op2.vs);
                } else if (ops2 == ">") {
                    res.vb = (op1.vs > op2.vs);
                } else {
                    IlAtom err;
                    err.t = ERROR;
                    err.vs = "BOOL-Cmp-" + ops2 + "-Wrong-Type-Operands";
                    pst->push_back(err);
                    return;
                }
            } else {
                IlAtom err;
                err.t = ERROR;
                err.vs = "Math-" + ops2 + "-Wrong-Type-Operands";
                pst->push_back(err);
                return;
            }
        } else {
            if (t1 == INT && t2 == INT) {
                res.t = BOOL;
                if (ops2 == "==") {
                    res.vb = (op1.vi == op2.vi);
                } else if (ops2 == "!=") {
                    res.vb = (op1.vi != op2.vi);
                } else if (ops2 == ">=") {
                    res.vb = (op1.vi >= op2.vi);
                } else if (ops2 == "<=") {
                    res.vb = (op1.vi <= op2.vi);
                } else if (ops2 == "<") {
                    res.vb = (op1.vi < op2.vi);
                } else if (ops2 == ">") {
                    res.vb = (op1.vi > op2.vi);
                } else {
                    IlAtom err;
                    err.t = ERROR;
                    err.vs = "INT-Cmp-" + ops2 + "-Wrong-Type-Operands";
                    pst->push_back(err);
                    return;
                }
            } else {
                res.t = BOOL;
                double o1, o2;
                if (op1.t == INT)
                    o1 = op1.vi;
                else
                    o1 = op1.vf;
                if (op2.t == INT)
                    o2 = op2.vi;
                else
                    o2 = op2.vf;
                if (ops2 == "==") {
                    res.vb = (o1 == o2);
                } else if (ops2 == "!=") {
                    res.vb = (o1 != o2);
                } else if (ops2 == ">=") {
                    res.vb = (o1 >= o2);
                } else if (ops2 == "<=") {
                    res.vb = (o1 <= o2);
                } else if (ops2 == "<") {
                    res.vb = (o1 < o2);
                } else if (ops2 == ">") {
                    res.vb = (o1 > o2);
                } else {
                    IlAtom err;
                    err.t = ERROR;
                    err.vs = "FLOAT-Cmp-" + ops2 + "-Wrong-Type-Operands";
                    pst->push_back(err);
                    return;
                }
            }
        }
        if (res.vb)
            res.vs = "true";
        else
            res.vs = "false";
        pst->push_back(res);
    }

    void dup(vector<IlAtom> *pst) {
        size_t l = pst->size();
        if (l < 1) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Stack-Underflow dup";
            pst->push_back(err);
            return;
        }
        IlAtom res = pst->back();
        pst->push_back(res);
        pst->push_back(res);
    }

    void dup2(vector<IlAtom> *pst) {
        size_t l = pst->size();
        if (l < 2) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Stack-Underflow dup2";
            pst->push_back(err);
            return;
        }
        IlAtom res2 = pst->back();
        pst->pop_back();
        IlAtom res = pst->back();
        pst->push_back(res2);
        pst->push_back(res);
        pst->push_back(res2);
    }

    void swap(vector<IlAtom> *pst) {
        size_t l = pst->size();
        if (l < 2) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Stack-Underflow swap";
            pst->push_back(err);
            return;
        }
        IlAtom res2 = pst->back();
        pst->pop_back();
        IlAtom res = pst->back();
        pst->pop_back();
        pst->push_back(res2);
        pst->push_back(res);
    }

    void drop(vector<IlAtom> *pst) {
        pst->pop_back();
    }

    void print(vector<IlAtom> *pst) {
        IlAtom res = pst->back();
        if (res.t == STRING)
            cout << res.vs;
        else
            cout << res.str();
        pst->pop_back();
    }

    void stack_size(vector<IlAtom> *pst) {
        size_t l = pst->size();
        IlAtom res;
        res.t = INT;
        res.vi = l;
        res.vs = std::to_string(l);
        pst->push_back(res);
    }

    void clear_stack(vector<IlAtom> *pst) {
        pst->clear();
    }

    IndraLink() {
        for (auto cm_op : "+-*/%") {
            if (cm_op == 0) continue;
            string m_op{cm_op};
            inbuilts[m_op] = [this, m_op](vector<IlAtom> *pst) { math_2ops(pst, m_op); };
        }
        for (auto cmp_op : {"==", "!=", ">=", "<=", "<", ">"}) {
            string m_op{cmp_op};
            inbuilts[m_op] = [this, m_op](vector<IlAtom> *pst) { cmp_2ops(pst, m_op); };
        }
        inbuilts["ss"] = [&](vector<IlAtom> *pst) { stack_size(pst); };
        inbuilts["cs"] = [&](vector<IlAtom> *pst) { clear_stack(pst); };
        inbuilts["dup"] = [&](vector<IlAtom> *pst) { dup(pst); };
        inbuilts["drop"] = [&](vector<IlAtom> *pst) { drop(pst); };
        inbuilts["dup2"] = [&](vector<IlAtom> *pst) { dup2(pst); };
        inbuilts["swap"] = [&](vector<IlAtom> *pst) { swap(pst); };
        inbuilts["."] = [&](vector<IlAtom> *pst) { print(pst); };
        inbuilts["print"] = [&](vector<IlAtom> *pst) { print(pst); };
        flow_control_words = {"for", "next", "if", "else", "endif", "while", "loop"};
        def_words = {":", ";"};
    }

    vector<string> split_old(const string &str, const string &delim) {
        // XXX: needs proper white-space treatment, comment and string handling! DOES NOT WORK LIKE THIS!
        vector<string> tokens;
        size_t prev = 0, pos = 0;
        do {
            pos = str.find(delim, prev);
            if (pos == string::npos) pos = str.length();
            string token = str.substr(prev, pos - prev);
            size_t posnl = token.find("\n");
            if (posnl != string::npos) {
                token = token.substr(0, posnl);
            }
            if (!token.empty()) tokens.push_back(token);
            prev = pos + delim.length();
        } while (pos < str.length() && prev < str.length());
        return tokens;
    }

    bool is_white_space(char c) {
        if (c == ' ' || c == '\n' || c == '\r' || c == '\t') return true;
        return false;
    }

    vector<string> split(const string &str, const string &delim) {
        // XXX: needs proper white-space treatment, comment and string handling! DOES NOT WORK LIKE THIS!
        vector<string> tokens;
        string tok;
        enum SplitState { START,
                          TOKEN,
                          WHITE_SPACE,
                          STRING,
                          STRING_ESC,
                          // STRING_END,
                          COMMENT1,
                          COMMENT2 };
        SplitState state = START;
        for (auto c : str) {
            switch (state) {
            case WHITE_SPACE:
                if (is_white_space(c)) {
                    continue;
                }
                // fall through:
            case START: {
                switch (c) {
                case '"':
                    state = STRING;
                    tok = c;
                    continue;
                case '(':
                    state = COMMENT1;
                    tok = c;
                    continue;
                case '\\':
                    state = COMMENT2;
                    tok = c;
                    continue;
                default:
                    state = TOKEN;
                    tok = c;
                    continue;
                }
            case TOKEN:
                if (is_white_space(c)) {
                    if (tok.length() > 0) {
                        tokens.push_back(tok);
                        tok = "";
                    }
                    state = WHITE_SPACE;
                    continue;
                } else {
                    tok += c;
                    continue;
                }
            case STRING_ESC:
                if (c == 'n') {
                    string sc = {10};
                    tok += sc;
                } else {
                    tok += c;
                }
                state = STRING;
                continue;
            case STRING:
                if (c == '\\') {
                    state = STRING_ESC;
                    continue;
                }
                if (c == '"') {
                    tok += c;
                    tokens.push_back(tok);
                    tok = "";
                    state = WHITE_SPACE;
                    continue;
                } else {
                    tok += c;
                    continue;
                }
            case COMMENT1:
                if (c == ')') {
                    tok += c;
                    tokens.push_back(tok);
                    tok = "";
                    state = WHITE_SPACE;
                    continue;
                } else {
                    tok += c;
                    continue;
                }
            case COMMENT2:
                if (c == '\n' || c == '\r') {
                    tokens.push_back(tok);
                    tok = "";
                    state = WHITE_SPACE;
                    continue;
                } else {
                    tok += c;
                    continue;
                }
            }
            }
        }
        return tokens;
    }

    bool is_int(string token, bool nat = false) {
        if (!nat) {
            if (token.length() && token[0] == '-')
                token = token.substr(1);
        }
        if (token.length() == 0)
            return false;
        string isn = "0123456789";
        for (unsigned int i = 0; i < token.length(); i++) {
            if (isn.find(token[i]) == string::npos) {
                return false;
            }
        }
        return true;
    }

    bool is_float(string token) {
        size_t pos = token.find('.');
        if (pos == string::npos) return false;
        string s1 = token.substr(0, pos);
        if (!is_int(s1)) return false;
        string s2 = token.substr(pos + 1);
        pos = s2.find('e');
        if (pos == string::npos) pos = s2.find('E');
        if (pos == string::npos) {
            if (!is_int(s2, true)) return false;
            return true;
        } else {
            string s1 = s2.substr(0, pos);
            if (!is_int(s1, true)) return false;
            string s3 = s2.substr(pos + 1);
            if (!is_int(s3)) return false;
            return true;
        }
    }

    bool is_comment(string token) {
        if (token.length() > 0 && (token[0] == '\\' || token[0] == '('))
            return true;
        else
            return false;
    }

    IlAtom parse_tok(string token) {
        IlAtom m;
        m.t = ERROR;
        m.vs = "Parse";

        if (is_comment(token)) {
            m.t = COMMENT;
            m.vs = token;
        } else if (is_def_word(token)) {
            m.t = DEF_WORD;
            m.vs = token;
        } else if (is_int(token)) {
            m.t = INT;
            m.vi = atoi(token.c_str());
            m.vs = token;
        } else if (is_float(token)) {
            m.t = FLOAT;
            m.vf = atof(token.c_str());
            m.vs = token;
        } else if (token == "false" || token == "true") {
            m.t = BOOL;
            m.vs = token;
            if (token == "false")
                m.vb = false;
            else
                m.vb = true;
        } else if (token.length() > 1 && token[0] == '"' && token[token.length() - 1] == '"') {
            m.t = STRING;
            m.vs = token.substr(1, token.length() - 2);
        } else if (is_flow_control(token)) {
            m.t = FLOW_CONTROL;
            m.vs = token;
            m.name = token;
        } else if (is_inbuilt(token)) {
            m.t = IFUNC;
            m.vif = inbuilts[token];
            m.vs = token;
        } else if (is_func(token)) {
            m.t = FUNC;
            m.name = token;
            m.vs = token;
        } else if (token.length() > 1 && token[0] == '>') {
            m.t = STORE_SYMBOL;
            m.name = token.substr(1);
            m.vs = token;
        } else if (token.length() > 1 && token[0] == '!') {
            m.t = DELETE_SYMBOL;
            m.name = token.substr(1);
            m.vs = token;
        } else {
            m.t = SYMBOL;
            m.name = token;
            m.vs = token;
        }
        return m;
    }

    vector<IlAtom> parse(string &input) {
        vector<IlAtom> ps;
        vector<string> tokens = split(input, " ");
        for (auto tok : tokens) {
            ps.push_back(parse_tok(tok));
        }
        return ps;
    }

    bool is_inbuilt(string funcName) {
        if (inbuilts.find(funcName) == inbuilts.end()) return false;
        return true;
    }

    bool is_func(string funcName) {
        if (funcs.find(funcName) == funcs.end()) return false;
        return true;
    }

    bool is_symbol(string symName) {
        if (symbols.find(symName) == symbols.end()) return false;
        return true;
    }

    bool is_flow_control(string symName) {
        if (std::find(flow_control_words.begin(), flow_control_words.end(), symName) == flow_control_words.end()) return false;
        return true;
    }

    bool is_def_word(string symName) {
        if (std::find(def_words.begin(), def_words.end(), symName) == def_words.end()) return false;
        return true;
    }

    bool is_reserved(string name) {
        return is_inbuilt(name) || is_flow_control(name) || is_def_word(name);
    }

    string store_def(vector<IlAtom> funcDef) {
        if (funcDef.size() < 2) {
            return "Func-Def-Too-Short: " + std::to_string(funcDef.size());
        }
        if (funcDef[0].t != SYMBOL) {
            return "Func-Def-First-Element-Must-be-Symbol";
        }
        string name = funcDef[0].vs;
        if (is_reserved(name)) {
            return "Func-Def-Name-is-reserved";
        }
        funcDef.erase(funcDef.begin());
        funcs[name] = funcDef;
        return "";
    }

    bool eval(vector<IlAtom> func, vector<IlAtom> *pst) {
        IlAtom res;
        bool abort = false;
        int pc = 0;
        vector<int> fc_stack;
        IlAtom ila;
        bool is_def = false;
        vector<IlAtom> funcDef;
        vector<IlAtom> newFunc;
        string err;
        vector<int> for_level, else_level, while_level, if_level;
        // Exctract function defintions:
        for (int pc = 0; pc < func.size(); pc++) {
            ila = func[pc];
            if (!is_def) {
                if (ila.t == DEF_WORD) {
                    if (ila.vs == ":") {
                        is_def = true;
                    } else if (ila.vs == ";") {
                        res.t = ERROR;
                        res.vs = "Def-End-Outside-Def";
                        abort = true;
                        pst->push_back(res);
                        break;
                    }
                } else {
                    newFunc.push_back(ila);
                }
            } else {
                if (ila.t == DEF_WORD) {
                    if (ila.vs == ":") {
                        res.t = ERROR;
                        res.vs = "Nested-Def-Illegal";
                        abort = true;
                        pst->push_back(res);
                        break;
                    } else if (ila.vs == ";") {
                        is_def = false;
                        err = store_def(funcDef);
                        if (err != "") {
                            res.t = ERROR;
                            res.vs = err;
                            abort = true;
                            pst->push_back(res);
                            break;
                        }
                        funcDef.clear();
                    }
                } else {
                    funcDef.push_back(ila);
                    cout << "funcDef: " << funcDef.size() << endl;
                }
            }
        }
        if (is_def) {
            res.t = ERROR;
            res.vs = "Unterminated-func-def";
            abort = true;
            pst->push_back(res);
        }
        // Search for branching instructions and establish jump targets:
        if (!abort) {
            for (int pc = 0; pc < newFunc.size(); pc++) {
                ila = newFunc[pc];
                if (ila.t == FLOW_CONTROL) {
                    if (ila.name == "for") {
                        if (pc < 3) {
                            res.t = ERROR;
                            res.vs = "Not enough stack data before 'for' instruction (3 required)";
                            abort = true;
                            pst->push_back(res);
                            break;
                        }
                        for_level.push_back(pc);
                    } else if (ila.name == "next") {
                        if (for_level.size() == 0) {
                            res.t = ERROR;
                            res.vs = "'next' without 'for'";
                            abort = true;
                            pst->push_back(res);
                            break;
                        }
                        int for_address = for_level.back();
                        newFunc[pc].jump_address = for_address;
                        newFunc[for_address].jump_address = pc;
                        for_level.pop_back();
                    } else if (ila.name == "if") {
                        if (pc < 1) {
                            res.t = ERROR;
                            res.vs = "Not enough stack data before 'if' instruction (1 required)";
                            abort = true;
                            pst->push_back(res);
                            break;
                        }
                        if_level.push_back(pc);
                        newFunc[pc].jump_address = 0;
                    } else if (ila.name == "else") {
                        if (if_level.size() - else_level.size() != 1) {
                            res.t = ERROR;
                            res.vs = "Unexpected 'else' statement!";
                            abort = true;
                            pst->push_back(res);
                            break;
                        }
                        int if_address = if_level.back();
                        newFunc[if_address].jump_address = pc;
                        else_level.push_back(if_address);
                    } else if (ila.name == "endif") {
                        if (if_level.size() == 0) {
                            res.t = ERROR;
                            res.vs = "'then' without 'if'";
                            abort = true;
                            pst->push_back(res);
                            break;
                        }
                        int if_address = if_level.back();
                        if (newFunc[if_address].jump_address == 0) {
                            newFunc[if_address].jump_address = pc;
                        } else {
                            int else_address = newFunc[if_address].jump_address;
                            newFunc[else_address].jump_address = pc;
                        }
                        if_level.pop_back();
                    } else if (ila.name == "while") {
                        if (pc < 1) {
                            res.t = ERROR;
                            res.vs = "Not enough stack data before 'while' instruction (1 required)";
                            abort = true;
                            pst->push_back(res);
                            break;
                        }
                        while_level.push_back(pc);
                    } else if (ila.name == "loop") {
                        if (while_level.size() == 0) {
                            res.t = ERROR;
                            res.vs = "'loop' without 'while'";
                            abort = true;
                            pst->push_back(res);
                            break;
                        }
                        int while_address = while_level.back();
                        newFunc[pc].jump_address = while_address;
                        newFunc[while_address].jump_address = pc;
                        while_level.pop_back();
                    }
                }
            }
        }
        if (!abort) {
            if (for_level.size() > 0) {
                res.t = ERROR;
                res.vs = "'for' without closing 'next''";
                abort = true;
                pst->push_back(res);
            } else if (if_level.size() > 0) {
                res.t = ERROR;
                res.vs = "'if' without closing 'endif''";
                abort = true;
                pst->push_back(res);
            }
        }
        // Eval:
        while (!abort && pc < newFunc.size()) {
            ila = newFunc[pc];
            // for (auto ila : func) {
            switch (ila.t) {
            case INT:
            case FLOAT:
            case BOOL:
            case STRING:
                pst->push_back(ila);
                break;
            case IFUNC:
                ila.vif(pst);
                if (pst->size() > 0) {
                    if ((*pst)[pst->size() - 1].t == ERROR) {
                        abort = true;
                    }
                }
                break;
            case FLOW_CONTROL:
                if (ila.name == "if") {
                    if (pst->size() == 0) {
                        res.t = ERROR;
                        res.vs = "Stack-underflow-on-if";
                        pst->push_back(res);
                        abort = true;
                    } else {
                        IlAtom b = pst->back();
                        pst->pop_back();
                        if (b.t != BOOL && b.t != INT) {
                            res.t = ERROR;
                            res.vs = "No-int-or-bool-for-if";
                            pst->push_back(res);
                            abort = true;
                        } else {
                            if (b.t == BOOL) {
                                if (b.vb) {
                                    break;
                                } else {
                                    pc = ila.jump_address;
                                }
                            } else if (b.t == INT) {
                                if (b.vi != 0) {
                                    break;
                                } else {
                                    pc = ila.jump_address;
                                }
                            }
                        }
                    }
                } else if (ila.name == "else") {
                    pc = ila.jump_address;
                } else if (ila.name == "endif") {
                } else if (ila.name == "while") {
                    if (pst->size() == 0) {
                        res.t = ERROR;
                        res.vs = "Stack-underflow-on-while";
                        pst->push_back(res);
                        abort = true;
                    } else {
                        IlAtom b = pst->back();
                        pst->pop_back();
                        if (b.t != BOOL && b.t != INT) {
                            res.t = ERROR;
                            res.vs = "No-int-or-bool-for-while";
                            pst->push_back(res);
                            abort = true;
                        } else {
                            if (b.t == BOOL) {
                                if (b.vb) {
                                    break;
                                } else {
                                    pc = ila.jump_address;
                                }
                            } else if (b.t == INT) {
                                if (b.vi != 0) {
                                    break;
                                } else {
                                    pc = ila.jump_address;
                                }
                            }
                        }
                    }
                } else if (ila.name == "loop") {
                    pc = ila.jump_address - 1;
                }
                break;
            case FUNC:
                if (is_func(ila.name)) {
                    eval(funcs[ila.name], pst);
                }
                break;
            case SYMBOL:
                if (is_symbol(ila.name)) {
                    IlAtom sym = symbols[ila.name];
                    switch (sym.t) {
                    case INT:
                        res.t = INT;
                        res.vi = sym.vi;
                        res.vs = std::to_string(sym.vi);
                        break;
                    case FLOAT:
                        res.t = FLOAT;
                        res.vf = sym.vf;
                        res.vs = std::to_string(sym.vf);
                        break;
                    case BOOL:
                        res.t = BOOL;
                        res.vb = sym.vb;
                        if (res.vb)
                            res.vs = "true";
                        else
                            res.vb = "false";
                        break;
                    case STRING:
                        res.t = STRING;
                        res.vs = sym.vs;
                        break;
                    default:
                        res.t = ERROR;
                        res.vs = "Illegal-Symbol-content-type";
                        abort = true;
                        break;
                    }
                    pst->push_back(res);
                } else {
                    res.t = ERROR;
                    res.vs = "Undefined-symbol-reference";
                    pst->push_back(res);
                    abort = true;
                }
                break;
            case STORE_SYMBOL:
                if (is_reserved(ila.name) || is_func(ila.name)) {
                    res.t = ERROR;
                    res.vs = "Name-in-use-by-func";
                    pst->push_back(res);
                    abort = true;
                    break;
                }
                if (pst->size() < 1) {
                    res.t = ERROR;
                    res.vs = "Symdef-stack-underflow";
                    pst->push_back(res);
                    abort = true;
                    break;
                }
                res = pst->back();
                pst->pop_back();
                if (res.t != INT && res.t != FLOAT && res.t && BOOL && res.t != STRING) {
                    res.t = ERROR;
                    res.vs = "Symdef-invalid-type";
                    pst->push_back(res);
                    abort = true;
                    break;
                }
                if (ila.name[0] == '>' || ila.name[0] == '!') {
                    res.t = ERROR;
                    res.vs = "Symdef-invalid-name";
                    pst->push_back(res);
                    abort = true;
                    break;
                }
                symbols[ila.name] = res;
                break;
            case DELETE_SYMBOL:
                if (!is_symbol(ila.name)) {
                    res.t = ERROR;
                    res.vs = "Symdelete-non-existant";
                    pst->push_back(res);
                    abort = true;
                    break;
                }
                symbols.erase(ila.name);
                break;
            case COMMENT:
                break;
            default:
                res.t = ERROR;
                res.vs = "Not-implemented";
                pst->push_back(res);
                abort = true;
                break;
            }
            ++pc;
        }
        if (abort) {
            if (pst->size() > 0 && (*pst)[pst->size() - 1].t == ERROR) {
                IlAtom err = pst->back();
                cout << err.str() << endl;
                pst->pop_back();
            } else {
                cout << endl
                     << "Terminated with error condition, but no error on stack!" << endl;
            }
        }

        return !abort;
    }
};

}  // namespace inlnk
