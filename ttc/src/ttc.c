#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>

// ELF64 types
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef struct {
    u8 ident[16];      // ELF identification
    u16 type;          // Object file type
    u16 machine;       // Machine type
    u32 version;       // Object file version
    u64 entry;         // Entry point address
    u64 phoff;         // Program header offset
    u64 shoff;         // Section header offset
    u32 flags;         // Processor-specific flags
    u16 ehsize;        // ELF header size
    u16 phentsize;     // Size of program header entry
    u16 phnum;         // Number of program header entries
    u16 shentsize;     // Size of section header entry
    u16 shnum;         // Number of section header entries
    u16 shstrndx;      // Section name string table index
} Elf64_Ehdr;

typedef struct {
    u32 type;          // Segment type
    u32 flags;         // Segment attributes
    u64 offset;        // Offset in file
    u64 vaddr;         // Virtual address in memory
    u64 paddr;         // Reserved
    u64 filesz;        // Size of segment in file
    u64 memsz;         // Size of segment in memory
    u64 align;         // Alignment of segment
} Elf64_Phdr;

// Error codes per spec Annex E
#define E0001 "E0001: reserved keyword as identifier"
#define E0012 "E0012: invalid literal"
#define E2001 "E2001: declaration outside namespace"
#define E5001 "E5001: illegal type or cast"

// Token kinds (based on spec keywords/types/operators)
enum TokenKind {
    TK_EOF, TK_IDENT, TK_NUMBER, TK_PLUS, TK_MINUS, TK_STAR, TK_SLASH, TK_LPAREN, TK_RPAREN,
    TK_SEMI, TK_COLON, TK_ARROW, TK_LBRACE, TK_RBRACE, TK_EQUAL, TK_RETURN, TK_VAR, TK_FN,
    TK_NAMESPACE, TK_INT32, TK_VOID, TK_TILDE // Add more as we expand
};

typedef struct Token {
    enum TokenKind kind;
    char *value; // For ident/number
    int line, col;
    struct Token *next;
} Token;

typedef struct Sym {
    char *name;
    char *type; // "int32" etc.
    int offset; // Stack offset for locals
    struct Sym *next;
} Sym;

typedef struct Node {
    enum { ND_NUM, ND_IDENT, ND_VAR, ND_RETURN, ND_BINOP, ND_UNARY, ND_BLOCK, ND_FN } kind;
    struct Node *lhs, *rhs, *body; // For binop/unary/block/fn
    char *name, *op, *type; // name for ident/var/fn, op for binop/unary
    int val; // For num
    struct Node *next; // For stmt lists
} Node;

// Global state
char *input;
int pos = 0, line = 1, col = 1;
Token *tokens = NULL, *cur_tok = NULL;
Sym *syms = NULL;
u8 *code_buf = NULL;
size_t code_len = 0, code_cap = 0;
int stack_size = 0;
FILE *out;

// Error reporting per §3.3
void error(char *code, char *msg, int l, int c) {
    fprintf(stderr, "error[%s]: %s (§3.x)--> file:%d:%d\nsuggestion: fix it\n", code, msg, l, c);
    exit(1);
}

// Append byte to code buffer
void emit_byte(u8 b) {
    if (code_len >= code_cap) {
        code_cap = code_cap ? code_cap * 2 : 1024;
        code_buf = realloc(code_buf, code_cap);
    }
    code_buf[code_len++] = b;
}

// Emit multi-byte (little-endian)
void emit_u32(u32 val) {
    emit_byte(val & 0xFF);
    emit_byte((val >> 8) & 0xFF);
    emit_byte((val >> 16) & 0xFF);
    emit_byte((val >> 24) & 0xFF);
}

void emit_u64(u64 val) {
    emit_u32(val & 0xFFFFFFFF);
    emit_u32(val >> 32);
}

// x86-64 encodings (simplified for our use, System V ABI)
void gen_expr(Node *n);

// Lexer (same as before)
Token *new_token(enum TokenKind k, char *v, int l, int c) {
    Token *t = malloc(sizeof(Token));
    t->kind = k; t->value = v ? strdup(v) : NULL; t->line = l; t->col = c; t->next = NULL;
    return t;
}

void add_token(enum TokenKind k, char *v) {
    Token *t = new_token(k, v, line, col);
    if (!tokens) tokens = t;
    else { Token *tmp = tokens; while (tmp->next) tmp = tmp->next; tmp->next = t; }
}

void lex() {
    while (input[pos]) {
        if (isspace((unsigned char)input[pos])) { if (input[pos++] == '\n') { line++; col = 1; } else col++; continue; }
        if (isdigit(input[pos])) { // Numbers (simple int for now, no bases/widths)
            char buf[32]; int i = 0; while (isdigit(input[pos])) buf[i++] = input[pos++]; buf[i] = 0;
            add_token(TK_NUMBER, buf); col += i; continue;
        }
        if (isalpha(input[pos]) || input[pos] == '_') { // Ident/keywords
            char buf[256]; int i = 0; while (isalnum(input[pos]) || input[pos] == '_') buf[i++] = input[pos++]; buf[i] = 0;
            if (!strcmp(buf, "namespace")) add_token(TK_NAMESPACE, NULL);
            else if (!strcmp(buf, "var")) add_token(TK_VAR, NULL);
            else if (!strcmp(buf, "fn")) add_token(TK_FN, NULL);
            else if (!strcmp(buf, "return")) add_token(TK_RETURN, NULL);
            else if (!strcmp(buf, "int32")) add_token(TK_INT32, NULL);
            else if (!strcmp(buf, "void")) add_token(TK_VOID, NULL);
            else add_token(TK_IDENT, buf);
            col += i; continue;
        }
        switch (input[pos]) {
            case '+': add_token(TK_PLUS, NULL); break;
            case '-': if (input[pos+1] == '>') { add_token(TK_ARROW, NULL); pos++; col++; } else add_token(TK_MINUS, NULL); break;
            case '*': add_token(TK_STAR, NULL); break;
            case '/': add_token(TK_SLASH, NULL); break;
            case '(': add_token(TK_LPAREN, NULL); break;
            case ')': add_token(TK_RPAREN, NULL); break;
            case '{': add_token(TK_LBRACE, NULL); break;
            case '}': add_token(TK_RBRACE, NULL); break;
            case ';': add_token(TK_SEMI, NULL); break;
            case ':': add_token(TK_COLON, NULL); break;
            case '=': add_token(TK_EQUAL, NULL); break;
            case '~': add_token(TK_TILDE, NULL); break;
            default: error("E0000", "unexpected char", line, col);
        }
        pos++; col++;
    }
    add_token(TK_EOF, NULL);
    cur_tok = tokens;
}

// Parser helpers
Token *consume(enum TokenKind k) {
    if (cur_tok->kind == k) { Token *t = cur_tok; cur_tok = cur_tok->next; return t; }
    return NULL;
}

Token *expect(enum TokenKind k) {
    Token *t = consume(k);
    if (!t) error("E0000", "unexpected token", cur_tok->line, cur_tok->col);
    return t;
}

// AST builders
Node *new_node(int k) {
    Node *n = calloc(1, sizeof(Node)); n->kind = k; return n;
}

Node *new_binop(char *op, Node *lhs, Node *rhs) {
    Node *n = new_node(ND_BINOP); n->op = op; n->lhs = lhs; n->rhs = rhs; return n;
}

Node *new_unary(char *op, Node *expr) {
    Node *n = new_node(ND_UNARY); n->op = op; n->lhs = expr; return n;
}

// Forward declarations for parser functions
Node *expr(void);
Node *add(void);
Node *mul(void);
Node *unary(void);
Node *primary(void);

Node *primary() {
    if (consume(TK_LPAREN)) { Node *n = expr(); expect(TK_RPAREN); return n; }
    Token *t = consume(TK_NUMBER); if (t) { Node *n = new_node(ND_NUM); n->val = atoi(t->value); return n; }
    t = consume(TK_IDENT); if (t) { Node *n = new_node(ND_IDENT); n->name = t->value; return n; }
    error("E0000", "expected expr", cur_tok->line, cur_tok->col); return NULL;
}

Node *unary() {
    if (consume(TK_PLUS)) return primary();
    if (consume(TK_MINUS)) return new_unary("-", primary());
    if (consume(TK_TILDE)) return new_unary("~", primary());
    return primary();
}

Node *mul() {
    Node *n = unary();
    for (;;) {
        if (consume(TK_STAR)) n = new_binop("*", n, unary());
        else if (consume(TK_SLASH)) n = new_binop("/", n, unary());
        else return n;
    }
}

Node *add() {
    Node *n = mul();
    for (;;) {
        if (consume(TK_PLUS)) n = new_binop("+", n, mul());
        else if (consume(TK_MINUS)) n = new_binop("-", n, mul());
        else return n;
    }
}

Node *expr() { return add(); }

Node *stmt() {
    if (consume(TK_RETURN)) {
        Node *n = new_node(ND_RETURN); n->lhs = expr(); expect(TK_SEMI); return n;
    }
    if (consume(TK_VAR)) {
        Token *name = expect(TK_IDENT); expect(TK_COLON); expect(TK_INT32);
        Node *n = new_node(ND_VAR); n->name = name->value; n->type = "int32";
        if (consume(TK_EQUAL)) n->lhs = expr();
        expect(TK_SEMI); return n;
    }
    Node *n = expr(); expect(TK_SEMI); return n;
}

Node *block() {
    Node *head = new_node(ND_BLOCK); Node *cur = head;
    while (!consume(TK_RBRACE)) {
        Node *s = stmt();
        cur->next = s;
        cur = s;
    }
    return head->next;
}

Node *fn() {
    Token *name = expect(TK_IDENT); expect(TK_LPAREN); expect(TK_RPAREN); expect(TK_ARROW);
    Token *ret = consume(TK_INT32) ? new_token(TK_INT32, "int32", 0, 0) : expect(TK_VOID);
    expect(TK_LBRACE);
    Node *n = new_node(ND_FN); n->name = name->value; n->type = ret->value; n->body = block();
    return n;
}

Node *namespace_decl() {
    expect(TK_IDENT); // Ignore name for now
    expect(TK_LBRACE);
    Node *head = new_node(ND_BLOCK); Node *cur = head;
    while (!consume(TK_RBRACE)) {
        if (consume(TK_FN)) { cur->next = fn(); cur = cur->next; }
        else error(E2001, "invalid decl", cur_tok->line, cur_tok->col);
    }
    return head->next;
}

Node *program() {
    expect(TK_NAMESPACE);
    return namespace_decl();
}

// Semantics
Sym *find_sym(char *name) {
    for (Sym *s = syms; s; s = s->next) if (!strcmp(s->name, name)) return s;
    return NULL;
}

void add_sym(char *name, char *type) {
    if (find_sym(name)) error("E0231", "duplicate symbol", line, col);
    Sym *s = malloc(sizeof(Sym)); s->name = strdup(name); s->type = strdup(type);
    s->offset = (stack_size += 8); // Use 8 bytes for int32 on x64, align
    s->next = syms; syms = s;
}

void sem_check(Node *n) {
    if (!n) return;
    switch (n->kind) {
        case ND_VAR: add_sym(n->name, n->type); sem_check(n->lhs); break;
        case ND_IDENT: if (!find_sym(n->name)) error("E2002", "undefined symbol", line, col); break;
        case ND_BINOP: case ND_UNARY: sem_check(n->lhs); sem_check(n->rhs); break;
        case ND_RETURN: sem_check(n->lhs); break;
        case ND_FN: syms = NULL; stack_size = 0; sem_check(n->body); break;
        default: break;
    }
    sem_check(n->next);
}

// Codegen to bytes (x86-64)
void gen_expr(Node *n) {
    if (n->kind == ND_NUM) {
        // mov rax, imm32 (sign-extend to 64)
        emit_byte(0x48); emit_byte(0xc7); emit_byte(0xc0); emit_u32(n->val); return;
    }
    if (n->kind == ND_IDENT) {
        Sym *s = find_sym(n->name);
        // mov rax, [rbp - offset] (disp32 if needed)
        emit_byte(0x48); emit_byte(0x8b); emit_byte(0x85); emit_u32(-s->offset); return;
    }
    if (n->kind == ND_UNARY) {
        gen_expr(n->lhs);
        if (!strcmp(n->op, "-")) { emit_byte(0x48); emit_byte(0xf7); emit_byte(0xd8); } // neg rax
        if (!strcmp(n->op, "~")) { emit_byte(0x48); emit_byte(0xf7); emit_byte(0xd0); } // not rax
        return;
    }
    if (n->kind == ND_BINOP) {
        gen_expr(n->rhs);
        emit_byte(0x50); // push rax
        gen_expr(n->lhs);
        emit_byte(0x5f); // pop rdi
        if (!strcmp(n->op, "+")) { emit_byte(0x48); emit_byte(0x01); emit_byte(0xf8); } // add rax, rdi
        else if (!strcmp(n->op, "-")) { emit_byte(0x48); emit_byte(0x29); emit_byte(0xf8); } // sub rax, rdi
        else if (!strcmp(n->op, "*")) { emit_byte(0x48); emit_byte(0xf7); emit_byte(0xef); } // imul rdi
        else if (!strcmp(n->op, "/")) {
            emit_byte(0x48); emit_byte(0x99); // cqo
            emit_byte(0x48); emit_byte(0xf7); emit_byte(0xff); // idiv rdi
        }
        return;
    }
}

void gen_stmt(Node *n) {
    if (n->kind == ND_VAR) {
        if (n->lhs) {
            gen_expr(n->lhs);
            Sym *s = find_sym(n->name);
            // mov [rbp - offset], rax
            emit_byte(0x48); emit_byte(0x89); emit_byte(0x85); emit_u32(-s->offset);
        }
        return;
    }
    if (n->kind == ND_RETURN) {
        gen_expr(n->lhs);
        // For now, syscall exit(rax)
        emit_byte(0x48); emit_byte(0x89); emit_byte(0xc7); // mov rdi, rax
        emit_byte(0xb8); emit_u32(60); // mov eax, 60 (exit)
        emit_byte(0x0f); emit_byte(0x05); // syscall
        return;
    }
    gen_expr(n); // expr_stmt, discard
}

void gen(Node *n) {
    if (n->kind == ND_FN) {
        // Prologue
        emit_byte(0x55); // push rbp
        emit_byte(0x48); emit_byte(0x89); emit_byte(0xe5); // mov rbp, rsp
        if (stack_size) {
            emit_byte(0x48); emit_byte(0x81); emit_byte(0xec); emit_u32(stack_size); // sub rsp, stack_size
        }

        // Body (stmts linked by next)
        for (Node *stmt = n->body; stmt; stmt = stmt->next) {
            gen_stmt(stmt);
        }

        // Epilogue
        emit_byte(0xc9); // leave
        emit_byte(0xc3); // ret
    }
}

int main(int argc, char **argv) {
    if (argc != 4 || strcmp(argv[2], "-o")) { printf("Usage: ttc input.tmg -o output\n"); return 1; }
    FILE *f = fopen(argv[1], "r");
    if (!f) return 1;
    fseek(f, 0, SEEK_END); long size = ftell(f); fseek(f, 0, SEEK_SET);
    input = malloc(size + 1); fread(input, 1, size, f); input[size] = 0; fclose(f);

    lex();
    Node *ast = program();
    sem_check(ast);

    // Generate code bytes
    gen(ast);

    // ELF setup
    const u64 base_addr = 0x400000;
    const u64 header_size = sizeof(Elf64_Ehdr) + sizeof(Elf64_Phdr);
    const u64 file_size = header_size + code_len;
    const u64 entry = base_addr + header_size;

    Elf64_Ehdr ehdr = {0};
    memcpy(ehdr.ident, "\177ELF\2\1\1\0\0\0\0\0\0\0\0\0", 16);
    ehdr.type = 2; // ET_EXEC
    ehdr.machine = 0x3E; // EM_X86_64
    ehdr.version = 1;
    ehdr.entry = entry;
    ehdr.phoff = sizeof(Elf64_Ehdr);
    ehdr.flags = 0;
    ehdr.ehsize = sizeof(Elf64_Ehdr);
    ehdr.phentsize = sizeof(Elf64_Phdr);
    ehdr.phnum = 1;

    Elf64_Phdr phdr = {0};
    phdr.type = 1; // PT_LOAD
    phdr.flags = 7; // PF_R | PF_W | PF_X
    phdr.offset = 0;
    phdr.vaddr = base_addr;
    phdr.paddr = base_addr;
    phdr.filesz = file_size;
    phdr.memsz = file_size;
    phdr.align = 0x1000;

    // Write binary
    out = fopen(argv[3], "wb");
    fwrite(&ehdr, sizeof(ehdr), 1, out);
    fwrite(&phdr, sizeof(phdr), 1, out);
    fwrite(code_buf, code_len, 1, out);
    fclose(out);
    free(code_buf);

    return 0;
}
