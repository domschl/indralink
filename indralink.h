#include <iostream>
#include <string>
#include <vector>
// #include <algorithm>
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
    vector<double> vaf;
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
            if (t1 == STRING && t2 == STRING && ops2 == "+") {
                res.t = STRING;
                res.vs = op1.vs + op2.vs;
            } else if (t1 == STRING && t2 == INT && op2.vi >= 0 && ops2 == "*") {
                res.t = STRING;
                res.vs = "";
                for (auto i = 0; i < op2.vi; i++)
                    res.vs += op1.vs;
            } else {
                IlAtom err;
                err.t = ERROR;
                err.vs = "Math-" + ops2 + "-Wrong-Type-Operands";
                pst->push_back(err);
                return;
            }
            pst->push_back(res);
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

    void
    cmp_2ops(vector<IlAtom> *pst, string ops2) {
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
        size_t l = pst->size();
        if (l < 1) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Stack-Underflow drop";
            pst->push_back(err);
            return;
        }
        pst->pop_back();
    }

    void range(vector<IlAtom> *pst) {
        size_t l = pst->size();
        if (l < 2) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Stack-Underflow range";
            pst->push_back(err);
            return;
        }
        IlAtom r1, r2;
        r2 = pst->back();
        pst->pop_back();
        r1 = pst->back();
        pst->pop_back();
        if (r1.t != INT || r2.t != INT) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Range required 2 INT args";
            pst->push_back(err);
            return;
        }
        IlAtom r;
        r.t = INT_ARRAY;
        if (r1.vi <= r2.vi) {
            for (auto i = r1.vi; i <= r2.vi; i++)
                r.vai.push_back(i);
        } else {
            for (auto i = r1.vi; i >= r2.vi; i--)
                r.vai.push_back(i);
        }
        pst->push_back(r);
    }

    void array_append(vector<IlAtom> *pst) {
        size_t l = pst->size();
        if (l < 2) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Stack-Underflow append";
            pst->push_back(err);
            return;
        }
        IlAtom r1, r2;
        r2 = pst->back();
        pst->pop_back();
        r1 = pst->back();
        pst->pop_back();
        if (r1.t == INT_ARRAY && r2.t == INT) {
            r1.vai.push_back(r2.vi);
            pst->push_back(r1);
        } else if (r1.t == FLOAT_ARRAY && r2.t == FLOAT) {
            r1.vaf.push_back(r2.vf);
            pst->push_back(r1);
        } else if (r1.t == BOOL_ARRAY && r2.t == BOOL) {
            r1.vab.push_back(r2.vb);
            pst->push_back(r1);
        } else if (r1.t == STRING_ARRAY && r2.t == STRING) {
            r1.vas.push_back(r2.vs);
            pst->push_back(r1);
        } else {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Append requires array and element of same type: INT, FLOAT, STRING, or BOOL";
            pst->push_back(err);
            return;
        }
        return;
    }

    void array_remove(vector<IlAtom> *pst) {
        size_t l = pst->size();
        if (l < 2) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Stack-Underflow remove";
            pst->push_back(err);
            return;
        }
        IlAtom r1, r2;
        r2 = pst->back();
        pst->pop_back();
        r1 = pst->back();
        pst->pop_back();
        if (r1.t == INT_ARRAY && r2.t == INT) {
            if (r2.vi >= r1.vai.size() || r2.vi < 0) {
                IlAtom err;
                err.t = ERROR;
                err.vs = "Index-out-of-range-on-remove";
                pst->push_back(err);
                return;
            }
            r1.vai.erase(r1.vai.begin() + r2.vi);
            pst->push_back(r1);
        } else if (r1.t == FLOAT_ARRAY && r2.t == INT) {
            if (r2.vi >= r1.vaf.size() || r2.vi < 0) {
                IlAtom err;
                err.t = ERROR;
                err.vs = "Index-out-of-range-on-remove";
                pst->push_back(err);
                return;
            }
            r1.vaf.erase(r1.vaf.begin() + r2.vi);
            pst->push_back(r1);
        } else if (r1.t == BOOL_ARRAY && r2.t == INT) {
            if (r2.vi >= r1.vab.size() || r2.vi < 0) {
                IlAtom err;
                err.t = ERROR;
                err.vs = "Index-out-of-range-on-remove";
                pst->push_back(err);
                return;
            }
            r1.vab.erase(r1.vab.begin() + r2.vi);
            pst->push_back(r1);
        } else if (r1.t == STRING_ARRAY && r2.t == INT) {
            if (r2.vi >= r1.vas.size() || r2.vi < 0) {
                IlAtom err;
                err.t = ERROR;
                err.vs = "Index-out-of-range-on-remove";
                pst->push_back(err);
                return;
            }
            r1.vas.erase(r1.vas.begin() + r2.vi);
            pst->push_back(r1);
        } else {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Remove requires array of type: INT, FLOAT, STRING, or BOOL and an Index of type INT";
            pst->push_back(err);
            return;
        }
        return;
    }

    void array_erase(vector<IlAtom> *pst) {
        size_t l = pst->size();
        if (l < 1) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Stack-Underflow remove";
            pst->push_back(err);
            return;
        }
        IlAtom r1;
        r1 = pst->back();
        pst->pop_back();
        if (r1.t == INT_ARRAY) {
            r1.vai.clear();
            pst->push_back(r1);
        } else if (r1.t == FLOAT_ARRAY) {
            r1.vaf.clear();
            pst->push_back(r1);
        } else if (r1.t == BOOL_ARRAY) {
            r1.vab.clear();
            pst->push_back(r1);
        } else if (r1.t == STRING_ARRAY) {
            r1.vas.clear();
            pst->push_back(r1);
        } else {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Erase requires array of type: INT, FLOAT, STRING, or BOOL";
            pst->push_back(err);
            return;
        }
        return;
    }

    void array_update(vector<IlAtom> *pst) {
        size_t l = pst->size();
        if (l < 3) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Stack-Underflow update";
            pst->push_back(err);
            return;
        }
        IlAtom r1, r2, r3;
        r3 = pst->back();
        pst->pop_back();
        r2 = pst->back();
        pst->pop_back();
        r1 = pst->back();
        pst->pop_back();
        if (r1.t == INT_ARRAY && r2.t == INT && r3.t == INT) {
            if (r2.vi >= r1.vai.size() || r2.vi < 0) {
                IlAtom err;
                err.t = ERROR;
                err.vs = "Index-out-of-range-on-update";
                pst->push_back(err);
                return;
            }
            r1.vai[r2.vi] = r3.vi;
            pst->push_back(r1);
        } else if (r1.t == FLOAT_ARRAY && r2.t == INT && r3.t == FLOAT) {
            if (r2.vi >= r1.vaf.size() || r2.vi < 0) {
                IlAtom err;
                err.t = ERROR;
                err.vs = "Index-out-of-range-on-update";
                pst->push_back(err);
                return;
            }
            r1.vaf[r2.vi] = r3.vf;
            pst->push_back(r1);
        } else if (r1.t == BOOL_ARRAY && r2.t == INT && r3.t == BOOL) {
            if (r2.vi >= r1.vab.size() || r2.vi < 0) {
                IlAtom err;
                err.t = ERROR;
                err.vs = "Index-out-of-range-on-update";
                pst->push_back(err);
                return;
            }
            r1.vab[r2.vi] = r3.vb;
            pst->push_back(r1);
        } else if (r1.t == STRING_ARRAY && r2.t == INT && r3.t == STRING) {
            if (r2.vi >= r1.vas.size() || r2.vi < 0) {
                IlAtom err;
                err.t = ERROR;
                err.vs = "Index-out-of-range-on-update";
                pst->push_back(err);
                return;
            }
            r1.vas[r2.vi] = r3.vs;
            pst->push_back(r1);
        } else {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Update requires array of type: INT, FLOAT, STRING, or BOOL and an Index of type INT, and a Value of same type as the array.";
            pst->push_back(err);
            return;
        }
        return;
    }

    void array_index(vector<IlAtom> *pst) {
        size_t l = pst->size();
        if (l < 2) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Stack-Underflow index";
            pst->push_back(err);
            return;
        }
        IlAtom r1, r2, res;
        r2 = pst->back();
        pst->pop_back();
        r1 = pst->back();
        pst->pop_back();
        if (r1.t == INT_ARRAY && r2.t == INT) {
            if (r2.vi >= r1.vai.size() || r2.vi < 0) {
                IlAtom err;
                err.t = ERROR;
                err.vs = "Index-out-of-range-on-index";
                pst->push_back(err);
                return;
            }
            res.t = INT;
            res.vi = r1.vai[r2.vi];
            res.vs = std::to_string(res.vi);
            pst->push_back(res);
        } else if (r1.t == FLOAT_ARRAY && r2.t == INT) {
            if (r2.vi >= r1.vaf.size() || r2.vi < 0) {
                IlAtom err;
                err.t = ERROR;
                err.vs = "Index-out-of-range-on-index";
                pst->push_back(err);
                return;
            }
            res.t = FLOAT;
            res.vf = r1.vaf[r2.vi];
            res.vs = std::to_string(res.vf);
            pst->push_back(res);
        } else if (r1.t == BOOL_ARRAY && r2.t == INT) {
            if (r2.vi >= r1.vab.size() || r2.vi < 0) {
                IlAtom err;
                err.t = ERROR;
                err.vs = "Index-out-of-range-on-index";
                pst->push_back(err);
                return;
            }
            res.t = BOOL;
            res.vb = r1.vab[r2.vi];
            if (res.vb)
                res.vs = "true";
            else
                res.vs = "false";
            pst->push_back(res);
        } else if (r1.t == STRING_ARRAY && r2.t == INT) {
            if (r2.vi >= r1.vas.size() || r2.vi < 0) {
                IlAtom err;
                err.t = ERROR;
                err.vs = "Index-out-of-range-on-index";
                pst->push_back(err);
                return;
            }
            res.t = STRING;
            res.vs = r1.vas[r2.vi];
            pst->push_back(res);
        } else if (r1.t == STRING && r2.t == INT) {
            if (r2.vi >= r1.vs.length() || r2.vi < 0) {
                IlAtom err;
                err.t = ERROR;
                err.vs = "Index-out-of-range-on-string-index";
                pst->push_back(err);
                return;
            }
            res.t = STRING;
            string cs{r1.vs[r2.vi]};
            res.vs = cs;
            pst->push_back(res);
        } else {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Update requires array of type: INT, FLOAT, STRING, or BOOL and an Index of type INT, and a Value of same type as the array.";
            pst->push_back(err);
            return;
        }
        return;
    }

    void array_sum(vector<IlAtom> *pst) {
        size_t l = pst->size();
        if (l < 1) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Stack-Underflow sum";
            pst->push_back(err);
            return;
        }
        IlAtom r1, res;
        r1 = pst->back();
        pst->pop_back();
        if (r1.t == INT_ARRAY) {
            res.t = INT;
            res.vi = 0;
            for (auto n : r1.vai)
                res.vi += n;
            res.vs = std::to_string(res.vi);
            pst->push_back(res);
        } else if (r1.t == FLOAT_ARRAY) {
            res.t = FLOAT;
            res.vf = 0.0;
            for (auto f : r1.vaf)
                res.vf += f;
            res.vs = std::to_string(res.vf);
            pst->push_back(res);
        } else if (r1.t == BOOL_ARRAY) {
            res.t = BOOL;
            res.vb = true;
            for (auto b : r1.vab)
                if (!b) res.vb = false;
            if (res.vb)
                res.vs = "true";
            else
                res.vs = "false";
            pst->push_back(res);
        } else if (r1.t == STRING_ARRAY) {
            res.t = STRING;
            res.vs = "";
            for (auto s : r1.vas)
                res.vs += s;
            pst->push_back(res);
        } else {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Sum requires array of type: INT, FLOAT, STRING, or BOOL";
            pst->push_back(err);
            return;
        }
        return;
    }

    void array_or_string_len(vector<IlAtom> *pst) {
        size_t l = pst->size();
        if (l < 1) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Stack-Underflow sum";
            pst->push_back(err);
            return;
        }
        IlAtom r1, res;
        r1 = pst->back();
        pst->pop_back();
        if (r1.t == INT_ARRAY) {
            res.t = INT;
            res.vi = r1.vai.size();
            res.vs = std::to_string(res.vi);
            pst->push_back(res);
        } else if (r1.t == FLOAT_ARRAY) {
            res.t = INT;
            res.vi = r1.vaf.size();
            res.vs = std::to_string(res.vi);
            pst->push_back(res);
        } else if (r1.t == BOOL_ARRAY) {
            res.t = INT;
            res.vi = r1.vab.size();
            res.vs = std::to_string(res.vi);
            pst->push_back(res);
        } else if (r1.t == STRING_ARRAY) {
            res.t = INT;
            res.vi = r1.vas.size();
            res.vs = std::to_string(res.vi);
            pst->push_back(res);
        } else if (r1.t == STRING) {
            res.t = INT;
            res.vi = r1.vs.length();
            res.vs = std::to_string(res.vi);
            pst->push_back(res);
        } else {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Sum requires array of type: INT, FLOAT, STRING, or BOOL";
            pst->push_back(err);
            return;
        }
        return;
    }
    void to_int(vector<IlAtom> *pst) {
        size_t l = pst->size();
        if (l < 1) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Stack-Underflow to_int";
            pst->push_back(err);
            return;
        }
        IlAtom r1, res;
        r1 = pst->back();
        pst->pop_back();
        if (r1.t == INT) {
            res.t = INT;
            res.vi = {r1.vi};
            res.vs = std::to_string(res.vi);
            pst->push_back(res);
        } else if (r1.t == FLOAT) {
            res.t = INT;
            res.vi = (int)r1.vf;
            res.vs = std::to_string(res.vi);
            pst->push_back(res);
        } else if (r1.t == BOOL) {
            res.t = INT;
            if (r1.t)
                res.vi = 1;
            else
                res.vi = 0;
            res.vs = std::to_string(res.vi);
            pst->push_back(res);
        } else if (r1.t == STRING) {
            if (is_int(r1.vs)) {
                res.t = INT;
                res.vi = atoi(r1.vs.c_str());
            } else {
                res.t = ERROR;
                res.vs = "Can't convert: " + r1.vs = " to int";
            }
            pst->push_back(res);
        } else {
            IlAtom err;
            err.t = ERROR;
            err.vs = "to_int requires: INT, FLOAT, STRING, or BOOL";
            pst->push_back(err);
            return;
        }
        return;
    }

    void to_float(vector<IlAtom> *pst) {
        size_t l = pst->size();
        if (l < 1) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Stack-Underflow to_float";
            pst->push_back(err);
            return;
        }
        IlAtom r1, res;
        r1 = pst->back();
        pst->pop_back();
        if (r1.t == INT) {
            res.t = FLOAT;
            res.vf = (double)r1.vi;
            res.vs = std::to_string(res.vf);
            pst->push_back(res);
        } else if (r1.t == FLOAT) {
            res.t = FLOAT;
            res.vf = r1.vf;
            res.vs = std::to_string(r1.vf);
            pst->push_back(res);
        } else if (r1.t == BOOL) {
            res.t = FLOAT;
            if (r1.t)
                res.vf = 1.0;
            else
                res.vf = 0.0;
            res.vs = std::to_string(res.vf);
            pst->push_back(res);
        } else if (r1.t == STRING) {
            if (is_float(r1.vs)) {
                res.t = FLOAT;
                res.vf = atof(r1.vs.c_str());
            } else {
                res.t = ERROR;
                res.vs = "Can't convert: " + r1.vs = " to float";
            }
            pst->push_back(res);
        } else {
            IlAtom err;
            err.t = ERROR;
            err.vs = "to_float requires: INT, FLOAT, STRING, or BOOL";
            pst->push_back(err);
            return;
        }
        return;
    }

    void to_bool(vector<IlAtom> *pst) {
        size_t l = pst->size();
        if (l < 1) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Stack-Underflow to_bool";
            pst->push_back(err);
            return;
        }
        IlAtom r1, res;
        r1 = pst->back();
        pst->pop_back();
        if (r1.t == INT) {
            res.t = BOOL;
            if (r1.vi) {
                res.vb = true;
                res.vs = "true";
            } else {
                res.vb = false;
                res.vs = "false";
            }
            pst->push_back(res);
        } else if (r1.t == FLOAT) {
            res.t = BOOL;
            if (r1.vf != 0.0) {
                res.vb = true;
                res.vs = "true";
            } else {
                res.vb = false;
                res.vs = "false";
            }
            pst->push_back(res);
        } else if (r1.t == BOOL) {
            res.t = BOOL;
            res.vb = r1.vb;
            if (r1.vb) {
                res.vs = "true";
            } else {
                res.vs = "false";
            }
            pst->push_back(res);
        } else if (r1.t == STRING) {
            res.t = BOOL;
            if (r1.vs == "true") {
                res.vb = true;
                res.vs = "true";
            } else {
                res.vb = false;
                res.vs = "false";
            }
            pst->push_back(res);
        } else {
            IlAtom err;
            err.t = ERROR;
            err.vs = "to_bool requires: INT, FLOAT, STRING, or BOOL";
            pst->push_back(err);
            return;
        }
        return;
    }

    void to_string(vector<IlAtom> *pst) {
        size_t l = pst->size();
        if (l < 1) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Stack-Underflow to_string";
            pst->push_back(err);
            return;
        }
        IlAtom r1, res;
        r1 = pst->back();
        pst->pop_back();
        if (r1.t == INT) {
            res.t = STRING;
            res.vs = std::to_string(r1.vi);
            pst->push_back(res);
        } else if (r1.t == FLOAT) {
            res.t = STRING;
            res.vs = std::to_string(r1.vf);
            pst->push_back(res);
        } else if (r1.t == BOOL) {
            res.t = STRING;
            if (r1.vb) {
                res.vs = "true";
            } else {
                res.vs = "false";
            }
            pst->push_back(res);
        } else if (r1.t == STRING) {
            res.t = STRING;
            res.vs = r1.vs;
            pst->push_back(res);
        } else {
            IlAtom err;
            err.t = ERROR;
            err.vs = "to_string requires: INT, FLOAT, STRING, or BOOL";
            pst->push_back(err);
            return;
        }
        return;
    }

    void to_array(vector<IlAtom> *pst) {
        size_t l = pst->size();
        if (l < 1) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Stack-Underflow to_array";
            pst->push_back(err);
            return;
        }
        IlAtom r1, res;
        r1 = pst->back();
        pst->pop_back();
        if (r1.t == INT) {
            res.t = INT_ARRAY;
            res.vai = {r1.vi};
            pst->push_back(res);
        } else if (r1.t == FLOAT) {
            res.t = FLOAT_ARRAY;
            res.vaf = {r1.vf};
            pst->push_back(res);
        } else if (r1.t == BOOL) {
            res.t = BOOL_ARRAY;
            res.vab = {r1.vb};
            pst->push_back(res);
        } else if (r1.t == STRING) {
            res.t = STRING_ARRAY;
            res.vas = {r1.vs};
            pst->push_back(res);
        } else {
            IlAtom err;
            err.t = ERROR;
            err.vs = "to_array requires: INT, FLOAT, STRING, or BOOL";
            pst->push_back(err);
            return;
        }
        return;
    }

    void string_split(vector<IlAtom> *pst) {
        size_t l = pst->size();
        if (l < 2) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Stack-Underflow string_split";
            pst->push_back(err);
            return;
        }
        IlAtom r1, r2;
        r2 = pst->back();
        pst->pop_back();
        r1 = pst->back();
        pst->pop_back();
        if (r1.t == STRING && r2.t == STRING) {
            string s = r1.vs;
            string sp = r2.vs;
            IlAtom r;
            r.t = STRING_ARRAY;
            if (sp == "") {
                for (auto c : s) {
                    string cs{c};
                    r.vas.push_back(cs);
                }
            } else {
                size_t p = s.find(sp);
                while (p != string::npos) {
                    r.vas.push_back(s.substr(0, p));
                    s = s.substr(p + sp.length());
                    p = s.find(sp);
                }
                if (s.length() > 0) r.vas.push_back(s);
            }
            pst->push_back(r);
        } else {
            IlAtom err;
            err.t = ERROR;
            err.vs = "string_split requires two STRING vars";
            pst->push_back(err);
            return;
        }
    }

    void string_substring(vector<IlAtom> *pst) {
        size_t l = pst->size();
        if (l < 3) {
            IlAtom err;
            err.t = ERROR;
            err.vs = "Stack-Underflow string_substring";
            pst->push_back(err);
            return;
        }
        IlAtom r1, r2, r3;
        r3 = pst->back();
        pst->pop_back();
        r2 = pst->back();
        pst->pop_back();
        r1 = pst->back();
        pst->pop_back();
        if (r1.t == STRING && r2.t == INT && r3.t == INT) {
            IlAtom r;
            r.t = STRING;
            if (r2.vi < 0 || r3.vi < 0 || r2.vi + r3.vi > r1.vs.length()) {
                IlAtom err;
                err.t = ERROR;
                err.vs = "string_substring index out-of-range";
                pst->push_back(err);
                return;
            }
            r.vs = r1.vs.substr(r2.vi, r3.vi);
            pst->push_back(r);
        } else {
            IlAtom err;
            err.t = ERROR;
            err.vs = "string_substring requires STRING, INT, INT";
            pst->push_back(err);
            return;
        }
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

    void list_vars(vector<IlAtom> *pst, map<string, IlAtom> *local_symbols = nullptr) {
        if (local_symbols) {
            cout << "--- Local ----------" << endl;
            for (const auto &symPair : *local_symbols) {
                IlAtom il = symPair.second;
                cout << il.str() << " >" << symPair.first << endl;
            }
        }
        cout << "--- Global ---------" << endl;
        for (const auto &symPair : symbols) {
            IlAtom il = symPair.second;
            cout << il.str() << " >" << symPair.first << endl;
        }
        cout << "--------------------" << endl;
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
        inbuilts["ps"] = [&](vector<IlAtom> *pst) { show_stack(pst); };
        inbuilts["listvars"] = [&](vector<IlAtom> *pst) { list_vars(pst); };
        inbuilts["listfuncs"] = [&](vector<IlAtom> *pst) { list_funcs(pst); };
        inbuilts["save"] = [&](vector<IlAtom> *pst) { save(pst); };
        inbuilts["load"] = [&](vector<IlAtom> *pst) { load(pst); };
        inbuilts["eval"] = [&](vector<IlAtom> *pst) { string_eval(pst); };
        inbuilts["range"] = [&](vector<IlAtom> *pst) { range(pst); };
        inbuilts["remove"] = [&](vector<IlAtom> *pst) { array_remove(pst); };
        inbuilts["append"] = [&](vector<IlAtom> *pst) { array_append(pst); };
        inbuilts["update"] = [&](vector<IlAtom> *pst) { array_update(pst); };
        inbuilts["index"] = [&](vector<IlAtom> *pst) { array_index(pst); };
        inbuilts["len"] = [&](vector<IlAtom> *pst) { array_or_string_len(pst); };
        inbuilts["erase"] = [&](vector<IlAtom> *pst) { array_erase(pst); };
        inbuilts["array"] = [&](vector<IlAtom> *pst) { to_array(pst); };
        inbuilts["int"] = [&](vector<IlAtom> *pst) { to_int(pst); };
        inbuilts["float"] = [&](vector<IlAtom> *pst) { to_float(pst); };
        inbuilts["bool"] = [&](vector<IlAtom> *pst) { to_bool(pst); };
        inbuilts["string"] = [&](vector<IlAtom> *pst) { to_string(pst); };
        inbuilts["split"] = [&](vector<IlAtom> *pst) { string_split(pst); };
        inbuilts["substring"] = [&](vector<IlAtom> *pst) { string_substring(pst); };
        inbuilts["sum"] = [&](vector<IlAtom> *pst) { array_sum(pst); };
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
            SYMBOL_TYPE syty;
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
                    if (el == "int") {
                        t = INT;
                        m.t = INT_ARRAY;
                        continue;
                    } else if (el == "float") {
                        t = FLOAT;
                        m.t = FLOAT_ARRAY;
                        continue;
                    } else if (el == "bool") {
                        t = BOOL;
                        m.t = BOOL_ARRAY;
                        continue;
                    } else if (el == "string") {
                        t = STRING;
                        m.t = STRING_ARRAY;
                        continue;
                    } else {
                        syty = symbol_type(el, nullptr);
                        if (syty != SYMBOL_TYPE::NONE) {
                            IlAtom sm;
                            switch (syty) {
                            // case SYMBOL_TYPE::LOCAL:
                            //    sm = local_symbols[el];
                            //    break;
                            case SYMBOL_TYPE::GLOBAL:
                                sm = symbols[el];
                                if (sm.t == INT || sm.t == FLOAT || sm.t == BOOL || sm.t == STRING) {
                                    ti = sm.t;
                                    el = sm.str();
                                }
                            }
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

    enum SYMBOL_TYPE { NONE,
                       LOCAL,
                       GLOBAL };

    SYMBOL_TYPE symbol_type(string symName, map<string, IlAtom> *local_symbols) {
        if (symName.length() < 1) return SYMBOL_TYPE::NONE;
        if (symName[0] != '$') {
            if ((local_symbols) && (local_symbols->find(symName) != local_symbols->end())) return SYMBOL_TYPE::LOCAL;
        }
        if (symName[0] == '$') {

            if (symbols.find(symName.substr(1)) != symbols.end()) return SYMBOL_TYPE::GLOBAL;

        } else {
            if (symbols.find(symName) != symbols.end()) return SYMBOL_TYPE::GLOBAL;
        }
        return SYMBOL_TYPE::NONE;
    }

    bool is_flow_control(string symName) {
        // if (flow_control_words.find(symName) == flow_control_words.end()) return false;
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

    bool eval(vector<IlAtom> func, vector<IlAtom> *pst, int *used_cycles = nullptr, int max_cycles = 0) {
        IlAtom res;
        bool abort = false;
        int pc = 0;
        vector<int> fc_stack;
        IlAtom ila;
        bool is_def = false;
        vector<IlAtom> funcDef;
        vector<IlAtom> newFunc;
        map<string, IlAtom> local_symbols;
        string err;
        vector<int> for_level, else_level, while_level, if_level;
        int cycles = 0;
        string last_loop = "";
        // Exctract function definitions:
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
                        if_level.push_back(pc);
                        else_level.push_back(-1);
                        newFunc[pc].jump_address = 0;
                    } else if (ila.name == "else") {
                        if (if_level.size() < 1 || else_level.back() != -1) {
                            res.t = ERROR;
                            res.vs = "Unexpected 'else' statement, e-level:" + std::to_string(else_level.back());
                            abort = true;
                            pst->push_back(res);
                            break;
                        }
                        int if_address = if_level.back();
                        newFunc[if_address].jump_address = pc;
                        else_level[else_level.size() - 1] = pc;
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
                        else_level.pop_back();
                    } else if (ila.name == "while") {
                        /*
                        if (pc < 1) {
                            res.t = ERROR;
                            res.vs = "Not enough stack data before 'while' instruction (1 required)";
                            abort = true;
                            pst->push_back(res);
                            break;
                        }
                        */
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
        if (!abort) {
            // Eval:
            last_loop = "";
            SYMBOL_TYPE syty;
            IlAtom sym;
            while (!abort && pc < newFunc.size()) {
                ++cycles;
                if (max_cycles && cycles > max_cycles) {
                    cout << endl
                         << "ABORT PROGRAM RUNTIME EXCEEDED" << endl;
                    abort = true;
                    sym.t = ERROR;
                    sym.vs = "Calculation exceeded max_cycles " + std::to_string(max_cycles) + ", aborted.";
                    pst->push_back(sym);
                    continue;
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
                        } else if (last_loop == "for") {
                            IlAtom for_array = pst->back();
                            pst->pop_back();
                            switch (for_array.t) {
                            case INT_ARRAY:
                                for_array.vai.clear();
                                pst->push_back(for_array);
                                break;
                            case FLOAT_ARRAY:
                                for_array.vaf.clear();
                                pst->push_back(for_array);
                                break;
                            case BOOL_ARRAY:
                                for_array.vab.clear();
                                pst->push_back(for_array);
                                break;
                            case STRING_ARRAY:
                                for_array.vas.clear();
                                pst->push_back(for_array);
                                break;
                            default:
                                res.t = ERROR;
                                res.vs = "Illegal array-type on for-break";
                                abort = 1;
                                pst->push_back(res);
                            }
                        }
                        // cout << "break";
                        pc = ila.jump_address - 1;
                    } else if (ila.name == "return") {
                        pc = newFunc.size();
                    }
                    break;
                case FUNC:
                    if (is_func(ila.name)) {
                        eval(funcs[ila.name], pst, used_cycles, max_cycles);
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
                    break;
                case SYMBOL:
                    syty = symbol_type(ila.name, &local_symbols);
                    sym.t = ERROR;
                    sym.vs = "Bad symbol type";
                    if (syty != SYMBOL_TYPE::NONE) {
                        if (syty == SYMBOL_TYPE::LOCAL)
                            sym = local_symbols[ila.name];
                        else if (syty == SYMBOL_TYPE::GLOBAL) {
                            if (ila.name[0] == '$') {
                                sym = symbols[ila.name.substr(1)];

                            } else {
                                sym = symbols[ila.name];
                            }
                        }
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
                        case FLOAT_ARRAY:
                            res.t = FLOAT_ARRAY;
                            res.vaf = sym.vaf;
                            res.vs = sym.str();
                            break;
                        case BOOL_ARRAY:
                            res.t = BOOL_ARRAY;
                            res.vab = sym.vab;
                            res.vs = sym.str();
                            break;
                        case STRING_ARRAY:
                            res.t = STRING_ARRAY;
                            res.vas = sym.vas;
                            res.vs = sym.str();
                            break;
                        case ERROR:
                            res = sym;
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
                    if (res.t != INT && res.t != FLOAT && res.t != BOOL && res.t != STRING && res.t != INT_ARRAY && res.t != FLOAT_ARRAY && res.t != BOOL_ARRAY && res.t != STRING_ARRAY) {
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
                    syty = symbol_type(ila.name, &local_symbols);
                    if (syty == SYMBOL_TYPE::GLOBAL)
                        if (ila.name[0] == '$') {
                            symbols[ila.name.substr(1)] = res;
                        } else {
                            symbols[ila.name] = res;
                        }
                    else if (ila.name[0] == '$') {
                        symbols[ila.name.substr(1)] = res;
                    } else {
                        local_symbols[ila.name] = res;
                    }
                    break;
                case DELETE_SYMBOL:
                    syty = symbol_type(ila.name, &local_symbols);
                    switch (syty) {
                    case SYMBOL_TYPE::NONE:
                        res.t = ERROR;
                        res.vs = "Symdelete-non-existant";
                        pst->push_back(res);
                        abort = true;
                        break;
                    case SYMBOL_TYPE::LOCAL:
                        local_symbols.erase(ila.name);
                        break;
                    case SYMBOL_TYPE::GLOBAL:
                        if (ila.name[0] == '$') {
                            symbols.erase(ila.name.substr(1));
                        } else {
                            symbols.erase(ila.name);
                        }
                        break;
                    }
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
        if (used_cycles) *used_cycles += cycles;
        return !abort;
    }
};

}  // namespace inlnk
