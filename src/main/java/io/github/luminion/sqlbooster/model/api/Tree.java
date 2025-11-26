package io.github.luminion.sqlbooster.model.api;

import io.github.luminion.sqlbooster.model.enums.SqlKeyword;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.*;

/**
 * SQL 条件树实体类.
 * <p>
 * 实现了 {@link Tree} 接口, 用于表示 SQL 查询条件的树形结构, 支持嵌套和复杂的查询逻辑.
 *
 * @author luminion
 * @since 1.0.0
 */
@Data
@NoArgsConstructor
public class Tree implements Iterable<Tree> {
    /**
     * 当前节点的条件列表.
     */
    protected LinkedHashSet<Condition> conditions = new LinkedHashSet<>();
    /**
     * 连接当前节点条件的逻辑连接符.
     */
    protected String connector = SqlKeyword.AND.getKeyword();
    /**
     * 子条件树.
     */
    protected Tree child;

    @Override
    public Iterator<Tree> iterator() {
        return new Itr(this);
    }

    public Tree appendConditions(Collection<Condition> conditions, String connector) {
        if (conditions == null || conditions.isEmpty()) {
            return this;
        }
        connector = SqlKeyword.replaceConnector(connector);
        if (SqlKeyword.OR.getKeyword().equals(connector)) {
            // append or conditions to last node
            Tree node = new Tree();
            node.conditions.addAll(conditions);
            node.connector = connector;
            Tree current = this;
            while (current.getChild() != null) {
                current = current.getChild();
            }
            current.child = node;
        } else {
            // put and conditions to current level
            this.getConditions().addAll(conditions);
        }
        return this;
    }

    protected Tree appendTree(Tree tree) {
        if (tree == null) {
            return this;
        }
        for (Tree node : tree) {
            if (node.getConditions().isEmpty()) {
                continue;
            }
            this.appendConditions(node.getConditions(), node.getConnector());
        }
        return this;
    }

    static class Itr implements Iterator<Tree> {

        private Tree current;

        public Itr(Tree root) {
            current = root;
        }

        @Override
        public boolean hasNext() {
            return current != null;
        }

        @Override
        public Tree next() {
            if (current == null) {
                throw new NoSuchElementException();
            }
            Tree result = current;
            current = current.getChild();
            return result;
        }
    }

}