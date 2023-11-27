#include <iostream>
#include <string>
#include <vector>
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
    FUNC,
    IFUNC,
    ERROR,
};

class IlAtom {
  public:
    ilAtomTypes t;
    int vi;
    double vf;
    string vs;
    bool vb;
    std::function<void(vector<IlAtom> *)> vif;

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
                IlAtom mod;
                mod.t = INT;
                mod.vi = o1 % o2;
                mod.vs = std::to_string(mod.vi);
                pst->push_back(mod);
                res.vi = o1 / o2;
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
            err.vs = "Stack-Underflow";
            pst->push_back(err);
            return;
        }
        IlAtom res = pst->back();
        pst->push_back(res);
        pst->push_back(res);
    }

    void pop(vector<IlAtom> *pst) {
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
        for (auto cm_op : "+-*/") {
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
        inbuilts["pop"] = [&](vector<IlAtom> *pst) { pop(pst); };
    }

    vector<string> split(const string &str, const string &delim) {
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

    IlAtom parse_tok(string token) {
        IlAtom m;
        m.t = ERROR;
        m.vs = "Parse";

        if (is_int(token)) {
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
        } else if (is_inbuilt(token)) {
            m.t = IFUNC;
            m.vif = inbuilts[token];
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

    bool eval(vector<IlAtom> func, vector<IlAtom> *pst) {
        for (auto ila : func) {
            switch (ila.t) {
            case INT:
            case FLOAT:
            case BOOL:
            case STRING:
                pst->push_back(ila);
                break;
            case IFUNC:
                ila.vif(pst);
                break;
            default:
                IlAtom err;
                err.t = ERROR;
                err.vs = "Not-implemented";
                pst->push_back(err);
                break;
            }
        }
        return true;
    }
};

}  // namespace inlnk
