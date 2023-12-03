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

/* Code bits and samples
Integer square-root, from: https://en.wikipedia.org/wiki/Integer_square_root
Takes integer and approximates best integer square root through iteration:
: isqrt (n -- sqrt n) dup dup 2 / dup2 != while dup2 dup >sqrt / + 2 / dup sqrt < loop drop drop drop sqrt ;
*/

namespace inlnk {

static string infSymbol = "∞";
static string fnSymbol = "⒡";

enum ilAtomTypes {
    UNDEFINED = 0,
    INT = 1,
    FLOAT,
    BOOL,
    STRING,
    INT_ARRAY,
    FLOAT_ARRAY,
    BOOL_ARRAY,
    STRING_ARRAY,
    SYMBOL,
    COMMENT,
    DEF_WORD,
    STORE_SYMBOL,
    DELETE_SYMBOL,
    FUNC,
    SHOW_FUNC,
    DELETE_FUNC,
    IFUNC,
    FLOW_CONTROL,
    ERROR,
};

void replaceAll(string &str, const string &from, const string &to) {
    // https://stackoverflow.com/questions/3418231/replace-part-of-a-string-with-another-string
    if (from.empty())
        return;
    size_t start_pos = 0;
    while ((start_pos = str.find(from, start_pos)) != string::npos) {
        str.replace(start_pos, from.length(), to);
        start_pos += to.length();
    }
}

class IlAtom {
  public:
    ilAtomTypes t;
    int vi;
    double vf;
    string vs;
    bool vb;
    vector<int> shape;
    vector<int> vai;
    vector<float> vaf;
    vector<string> vas;
    vector<bool> vab;
    string name;
    std::function<void(vector<IlAtom> *)> vif;
    int jump_address;

    IlAtom() {
        t = ERROR;
        vs = "Not-Init";
    }

    string str() {
        string ir;
        switch (t) {
        case INT:
        case FLOAT:
        case BOOL:
            return vs;
            break;
        case STRING:
            ir = '"' + vs + '"';
            replaceAll(ir, "\n", "\\n");
            return ir;
            break;
        case INT_ARRAY:
            ir = "[ ";
            for (auto i : vai) {
                ir += std::to_string(i) + " ";
            }
            ir += "]";
            return ir;
            break;
        case FLOAT_ARRAY:
            ir = "[ ";
            for (auto f : vaf) {
                ir += std::to_string(f) + " ";
            }
            ir += "]";
            return ir;
            break;
        case BOOL_ARRAY:
            ir = "[ ";
            for (auto b : vab) {
                if (b)
                    ir += "true ";
                else
                    ir += "false ";
            }
            ir += "]";
            return ir;
            break;
        case STRING_ARRAY:
            ir = "[ ";
            for (auto s : vas) {
                ir += '"' + s + '"' + " ";
            }
            ir += "]";
            return ir;
            break;
        case IFUNC:
        case FUNC:
        case SHOW_FUNC:
        case DELETE_FUNC:
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

    void bool_2ops(vector<IlAtom> *pst, string ops2) {
        size_t l = pst->size();
        if (l < 2) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Bool-Not-Enough-Operands";
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

        if ((t1 != INT && t1 != BOOL) || (t2 != INT && t2 != BOOL)) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Bool-requires-int-or-bool-Operands";
            pst->push_back(err);
            return;
        }
        bool b1, b2;
        if (t1 == BOOL)
            b1 = op1.vb;
        else {
            if (op1.vi == 0)
                b1 = false;
            else
                b1 = true;
        }
        if (t2 == BOOL)
            b2 = op2.vb;
        else {
            if (op2.vi == 0)
                b2 = false;
            else
                b2 = true;
        }
        res.t = BOOL;
        if (ops2 == "and")
            res.vb = (b1 && b2);
        else if (ops2 == "or")
            res.vb = (b1 || b2);
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

    void show_stack(vector<IlAtom> *pst) {
        cout << "[";
        bool first = true;
        for (auto il : *pst) {
            if (first) {
                first = false;
            } else {
                cout << ", ";
            }
            cout << il.str();
        }
        cout << "]" << endl;
    }

    void clear_stack(vector<IlAtom> *pst) {
        pst->clear();
    }

    void list_vars(vector<IlAtom> *pst) {
        for (const auto &symPair : symbols) {
            IlAtom il = symPair.second;
            cout << il.str() << " >" << symPair.first << endl;
        }
    }

    void list_funcs(vector<IlAtom> *pst) {
        for (const auto &funcPair : funcs) {
            show_func(funcPair.first);
        }
    }

    void save(vector<IlAtom> *pst) {
        size_t l = pst->size();
        if (l < 1) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Stack-Underflow-no-filename-on-save";
            pst->push_back(err);
            return;
        }
        IlAtom filedesc = pst->back();
        pst->pop_back();
        if (filedesc.t != STRING) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "filename-must-be-string-on-save";
            pst->push_back(err);
            return;
        }
        FILE *fp = fopen(filedesc.vs.c_str(), "w");
        if (fp) {
            for (const auto &funcPair : funcs) {
                string name = funcPair.first;
                vector<IlAtom> func = funcPair.second;
                fprintf(fp, ": %s ", name.c_str());
                for (auto il : func) {
                    string enc_line = il.str();
                    // replaceAll(enc_line, "\\", "\\\\");
                    fprintf(fp, "%s ", enc_line.c_str());
                }
                fprintf(fp, ";\n");
            }
            fclose(fp);
        }
    }

    void load(vector<IlAtom> *pst) {
        size_t l = pst->size();
        if (l < 1) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Stack-Underflow-no-filename-on-load";
            pst->push_back(err);
            return;
        }
        IlAtom filedesc = pst->back();
        pst->pop_back();
        if (filedesc.t != STRING) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "filename-must-be-string-on-load";
            pst->push_back(err);
            return;
        }
        char buf[129];
        int nb;
        string cmd = "";
        FILE *fp = fopen(filedesc.vs.c_str(), "r");
        if (fp) {
            while (!feof(fp)) {
                nb = fread(buf, 1, 128, fp);
                buf[nb] = 0;
                cmd += buf;
            }
        }
        replaceAll(cmd, "\\n", "\n");
        vector<IlAtom> ps = parse(cmd);
        eval(ps, pst);
    }

    void string_eval(vector<IlAtom> *pst) {
        size_t l = pst->size();
        if (l < 1) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Stack-Underflow-no-filename-on-dyn-eval";
            pst->push_back(err);
            return;
        }
        IlAtom ila = pst->back();
        pst->pop_back();
        if (ila.t != STRING) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Dyn-eval-requires-string-argument";
            pst->push_back(err);
            return;
        }
        string cmd = ila.vs;
        // replaceAll(cmd, "\\n", "\n");
        vector<IlAtom> ps = parse(cmd);
        eval(ps, pst);
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
        for (auto bool_op : {"and", "or"}) {
            string m_op{bool_op};
            inbuilts[m_op] = [this, m_op](vector<IlAtom> *pst) { bool_2ops(pst, m_op); };
        }
        inbuilts["ss"] = [&](vector<IlAtom> *pst) { stack_size(pst); };
        inbuilts["cs"] = [&](vector<IlAtom> *pst) { clear_stack(pst); };
        inbuilts["dup"] = [&](vector<IlAtom> *pst) { dup(pst); };
        inbuilts["drop"] = [&](vector<IlAtom> *pst) { drop(pst); };
        inbuilts["dup2"] = [&](vector<IlAtom> *pst) { dup2(pst); };
        inbuilts["swap"] = [&](vector<IlAtom> *pst) { swap(pst); };
        inbuilts["."] = [&](vector<IlAtom> *pst) { print(pst); };
        inbuilts["print"] = [&](vector<IlAtom> *pst) { print(pst); };
        inbuilts["printstack"] = [&](vector<IlAtom> *pst) { show_stack(pst); };
        inbuilts["listvars"] = [&](vector<IlAtom> *pst) { list_vars(pst); };
        inbuilts["listfuncs"] = [&](vector<IlAtom> *pst) { list_funcs(pst); };
        inbuilts["save"] = [&](vector<IlAtom> *pst) { save(pst); };
        inbuilts["load"] = [&](vector<IlAtom> *pst) { load(pst); };
        inbuilts["eval"] = [&](vector<IlAtom> *pst) { string_eval(pst); };
        flow_control_words = {"for", "next", "if", "else", "endif", "while", "loop", "break", "return"};
        def_words = {":", ";"};
    }

    bool is_white_space(char c) {
        if (c == ' ' || c == '\n' || c == '\r' || c == '\t') return true;
        return false;
    }

    vector<string> split(const string &str) {
        vector<string> tokens;
        string tok;
        enum SplitState { START,
                          TOKEN,
                          WHITE_SPACE,
                          STRING,
                          STRING_ESC,
                          // STRING_END,
                          ARRAY,
                          COMMENT1,
                          COMMENT2 };
        SplitState state = START;
        for (auto c : str + " ") {
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
                case '[':
                    state = ARRAY;
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
            case ARRAY:
                if (c == ']') {
                    tok += c;
                    tokens.push_back(tok);
                    tok = "";
                    state = WHITE_SPACE;
                    continue;
                } else {
                    tok += c;
                    continue;
                }
            case STRING_ESC:
                if (c == 'n') {  // XXX other escs?
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

    bool is_bool(string token) {
        if (token == "false" || token == "true")
            return true;
        else
            return false;
    }

    bool is_string(string token) {
        if (token.length() > 1 && token[0] == '"' && token[token.length() - 1] == '"')
            return true;
        else
            return false;
    }

    bool is_array(string token) {
        if (token.length() > 1 && token[0] == '[' && token[token.length() - 1] == ']')
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
        } else if (is_bool(token)) {
            m.t = BOOL;
            m.vs = token;
            if (token == "false")
                m.vb = false;
            else
                m.vb = true;
        } else if (is_string(token)) {
            m.t = STRING;
            m.vs = token.substr(1, token.length() - 2);
        } else if (is_array(token)) {
            string arr = token.substr(1, token.length() - 2);
            vector<string> arr_els = split(arr);
            ilAtomTypes t = UNDEFINED;
            ilAtomTypes ti;
            for (auto el : arr_els) {
                ti = UNDEFINED;
                if (is_comment(el)) continue;
                if (is_int(el))
                    ti = INT;
                else if (is_float(el))
                    ti = FLOAT;
                else if (is_bool(el))
                    ti = BOOL;
                else if (is_string(el))
                    ti = STRING;
                if (ti == UNDEFINED) {
                    if (symbols.find(el) != symbols.end()) {
                        IlAtom sm = symbols[el];
                        if (sm.t == INT || sm.t == FLOAT || sm.t == BOOL || sm.t == STRING) {
                            ti = sm.t;
                            el = sm.str();
                        }
                    }
                }
                if (t == UNDEFINED && ti != UNDEFINED) t = ti;
                if (t != ti || t == UNDEFINED) {
                    m.t = ERROR;
                    m.vs = "Bad-array-el: " + el;
                    break;
                }
                switch (t) {
                case INT:
                    m.vai.push_back(atoi(el.c_str()));
                    m.t = INT_ARRAY;
                    break;
                case FLOAT:
                    m.vaf.push_back(atof(el.c_str()));
                    m.t = FLOAT_ARRAY;
                    break;
                case BOOL:
                    if (el == "true")
                        m.vab.push_back(true);
                    else
                        m.vab.push_back(false);
                    m.t = BOOL_ARRAY;
                    break;
                case STRING:
                    if (el.length() > 1)
                        el = el.substr(1, el.length() - 2);
                    else
                        el = "INV_STR";
                    m.vas.push_back(el);
                    m.t = STRING_ARRAY;
                    break;
                default:
                    m.t = ERROR;
                    m.vs = "BAD_Array_state_type";
                    t = ERROR;
                    break;
                }
            }
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
        } else if (token.length() > 1 && token[0] == '?' && is_func(token.substr(1))) {
            m.t = SHOW_FUNC;
            m.name = token.substr(1);
            m.vs = token;
        } else if (token.length() > 1 && token[0] == '!' && is_func(token.substr(1))) {
            m.t = DELETE_FUNC;
            m.name = token.substr(1);
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
        vector<string> tokens = split(input);
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
        if (funcDef[0].t != SYMBOL && funcDef[0].t != FUNC) {
            return "Func-Def-First-Element-Must-be-Symbol";
        }
        string name = funcDef[0].vs;
        if (is_reserved(name)) {
            return "Func-Def-Name-is-reserved";
        }
        if (name[0] == '?' || name[0] == '!') {
            return "Illegal-Func-Def-name-first-char";
        }
        funcDef.erase(funcDef.begin());
        funcs[name] = funcDef;
        return "";
    }

    void show_func(string name) {
        vector<IlAtom> func = funcs[name];
        cout << ": " << name << " ";
        for (auto il : func) {
            cout << il.str() << " ";
        }
        cout << ";" << endl;
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
        int cycles = 0;
        string last_loop = "";
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
                        funcDef.clear();
                        if (err != "") {
                            res.t = ERROR;
                            res.vs = err;
                            abort = true;
                            pst->push_back(res);
                            break;
                        }
                    }
                } else {
                    funcDef.push_back(ila);
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
                        if (pc < 1) {
                            res.t = ERROR;
                            res.vs = "Not enough stack data before 'for' instruction (1 required)";
                            abort = true;
                            pst->push_back(res);
                            break;
                        }
                        for_level.push_back(pc);
                        last_loop = "for";
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
                        last_loop = "while";
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
                    } else if (ila.name == "break") {
                        if (last_loop == "while") {
                            if (while_level.size() == 0) {
                                res.t = ERROR;
                                res.vs = "'break' without 'while'";
                                abort = true;
                                pst->push_back(res);
                                break;
                            }
                            int while_address = while_level.back();
                            newFunc[pc].jump_address = while_address;
                        } else if (last_loop == "for") {
                            if (for_level.size() == 0) {
                                res.t = ERROR;
                                res.vs = "'break' without 'for'";
                                abort = true;
                                pst->push_back(res);
                                break;
                            }
                            int for_address = for_level.back();
                            newFunc[pc].jump_address = for_address;
                        } else {
                            res.t = ERROR;
                            res.vs = "'break' without 'for' or 'while'";
                            abort = true;
                            pst->push_back(res);
                            break;
                        }
                    } else if (ila.name == "return") {
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
        last_loop = "";
        while (!abort && pc < newFunc.size()) {
            ++cycles;
            if (cycles > 2500) {
                cout << endl
                     << "ABORT PROGRAM RUNTIME EXCEEDED" << endl;
                abort = true;
            }

            ila = newFunc[pc];
            // for (auto ila : func) {
            switch (ila.t) {
            case INT:
            case FLOAT:
            case BOOL:
            case STRING:
                pst->push_back(ila);
                break;
            case INT_ARRAY:
            case FLOAT_ARRAY:
            case BOOL_ARRAY:
            case STRING_ARRAY:
                if (ila.t == ERROR) {
                    abort = true;
                } else {
                    pst->push_back(ila);
                }
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
                        last_loop = "while";
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
                } else if (ila.name == "for") {
                    if (pst->size() == 0) {
                        res.t = ERROR;
                        res.vs = "Stack-underflow-on-for";
                        pst->push_back(res);
                        abort = true;
                    } else {
                        IlAtom b = pst->back();
                        pst->pop_back();
                        if (b.t != INT_ARRAY && b.t != STRING_ARRAY && b.t != FLOAT_ARRAY && b.t != BOOL_ARRAY) {
                            res.t = ERROR;
                            res.vs = "'for' requires an INT, STRING, FLOAT, or BOOL array stack";
                            pst->push_back(res);
                            abort = true;
                        } else {
                            last_loop = "for";
                            switch (b.t) {
                            case INT_ARRAY:
                                if (b.vai.size() == 0) {
                                    pc = ila.jump_address;
                                } else {
                                    IlAtom fi;
                                    fi.t = INT;
                                    fi.vi = b.vai[0];
                                    fi.vs = std::to_string(fi.vi);
                                    b.vai.erase(b.vai.begin());
                                    pst->push_back(b);
                                    pst->push_back(fi);
                                }
                                break;
                            case FLOAT_ARRAY:
                                if (b.vaf.size() == 0) {
                                    pc = ila.jump_address;
                                } else {
                                    IlAtom fi;
                                    fi.t = FLOAT;
                                    fi.vf = b.vaf[0];
                                    fi.vs = std::to_string(fi.vf);
                                    b.vaf.erase(b.vaf.begin());
                                    pst->push_back(b);
                                    pst->push_back(fi);
                                }
                                break;
                            case BOOL_ARRAY:
                                if (b.vab.size() == 0) {
                                    pc = ila.jump_address;
                                } else {
                                    IlAtom fi;
                                    fi.t = BOOL;
                                    fi.vb = b.vab[0];
                                    if (fi.vb)
                                        fi.vs = "true";
                                    else
                                        fi.vs = "false";
                                    b.vab.erase(b.vab.begin());
                                    pst->push_back(b);
                                    pst->push_back(fi);
                                }
                                break;
                            case STRING_ARRAY:
                                if (b.vas.size() == 0) {
                                    pc = ila.jump_address;
                                } else {
                                    IlAtom fi;
                                    fi.t = STRING;
                                    fi.vs = b.vas[0];
                                    b.vas.erase(b.vas.begin());
                                    pst->push_back(b);
                                    pst->push_back(fi);
                                }
                                break;
                            default:
                                res.t = ERROR;
                                res.vs = "'for' encounter illegal array type";
                                pst->push_back(res);
                                abort = true;
                                break;
                            }
                        }
                    }
                } else if (ila.name == "next") {
                    pc = ila.jump_address - 1;
                } else if (ila.name == "break") {
                    if (last_loop == "while") {
                        res.t = BOOL;
                        res.vb = false;
                        pst->push_back(res);
                    }
                    pc = ila.jump_address - 1;
                } else if (ila.name == "return") {
                    pc = newFunc.size();
                }
                break;
            case FUNC:
                if (is_func(ila.name)) {
                    eval(funcs[ila.name], pst);
                } else {
                    res.t = ERROR;
                    res.vs = "Func-does-not-exist: " + ila.name;
                    abort = true;
                    break;
                }
                break;
            case SHOW_FUNC:
                if (is_func(ila.name)) {
                    show_func(ila.name);
                } else {
                    res.t = ERROR;
                    res.vs = "Func-does-not-exist: " + ila.name;
                    abort = true;
                    break;
                }
                break;
            case DELETE_FUNC:
                if (is_func(ila.name)) {
                    funcs.erase(ila.name);
                } else {
                    res.t = ERROR;
                    res.vs = "Func-does-not-exist: " + ila.name;
                    abort = true;
                    break;
                }
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
                    case INT_ARRAY:
                        res.t = INT_ARRAY;
                        res.vai = sym.vai;
                        res.vs = sym.str();
                        break;
                    default:
                        res.t = ERROR;
                        res.vs = "Illegal-Symbol-content-type";
                        abort = true;
                        break;
                    }
                    pst->push_back(res);
                } else {
                    if (is_func(ila.name)) {  // If a function gets defined during current command, it might have been parsed at unknown symbol
                        eval(funcs[ila.name], pst);
                    } else {
                        res.t = ERROR;
                        res.vs = "Undefined-symbol-reference: <" + ila.name + ">";
                        pst->push_back(res);
                        abort = true;
                    }
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
                if (res.t != INT && res.t != FLOAT && res.t && BOOL && res.t != STRING && res.t != INT_ARRAY && res.t != FLOAT_ARRAY && res.t != BOOL_ARRAY && res.t != STRING_ARRAY) {
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
