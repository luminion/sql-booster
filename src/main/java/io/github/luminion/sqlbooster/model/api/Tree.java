package io.github.luminion.sqlbooster.model.api;

import io.github.luminion.sqlbooster.model.enums.SqlKeyword;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.NoSuchElementException;

/**
 * SQL 条件树实体类.
 * <p>
 * 实现了 {@link Tree} 接口, 用于表示 SQL 查询条件的树形结构, 支持嵌套和复杂的查询逻辑.
 *
 * @author luminion
 * @since 1.0.0
 */
@Getter
@ToString
@EqualsAndHashCode
@NoArgsConstructor
public class Tree implements Iterable<Tree> {
    /**
     * 当前节点的条件列表.
     */
    protected Collection<Condition> conditions;
    /**
     * 连接当前节点条件的逻辑连接符.
     */
    protected String connector;
    /**
     * 子条件树.
     */
    protected Tree child;

    {
        this.conditions = new LinkedHashSet<>();
        connector = SqlKeyword.AND.getKeyword();
    }


    /**
     * 使用指定的条件集合构造一个新的 {@link Tree}.
     *
     * @param conditions 条件集合
     * @since 1.0.0
     */
    public Tree(Collection<Condition> conditions) {
        this.conditions.addAll(conditions);
    }

    /**
     * 使用指定的条件集合和连接符构造一个新的 {@link Tree}.
     *
     * @param conditions 条件集合
     * @param connector  逻辑连接符 (AND/OR)
     * @throws IllegalArgumentException 如果连接符不是 AND 或 OR
     * @since 1.0.0
     */
    public Tree(Collection<Condition> conditions, String connector) {
        this.conditions.addAll(conditions);
        this.connector = SqlKeyword.replaceConnector(connector);
    }

    /**
     * 添加一个子节点到当前树的末端.
     *
     * @param child 要添加的子树
     * @return 当前 {@link Tree} 实例
     * @since 1.0.0
     */
    private Tree addSingleChild(Tree child) {
        if (child == null || child.getConditions().isEmpty()) {
            return this;
        }
        // create a new child to avoid circling reference
        Tree newChild = new Tree(child.getConditions(), SqlKeyword.OR.getKeyword());
        Tree current = this;
        while (current.getChild() != null) {
            current = current.getChild();
        }
        current.child = newChild;
        return this;
    }

    /**
     * 将另一个 {@link Tree} 合并到当前树中.
     *
     * @param sqlTree 要合并的 SQL 树
     * @return 当前 {@link Tree} 实例
     * @since 1.0.0
     */
    protected Tree addChild(Tree sqlTree) {
        if (sqlTree == null) {
            return this;
        }
        for (Tree node : sqlTree) {
            if (node.getConditions().isEmpty()) {
                continue;
            }
            String symbol1 = node.getConnector();
            if (SqlKeyword.OR.getKeyword().equals(symbol1)) {
                // put or conditions as child
                this.addSingleChild(node);
            } else {
                // put and conditions to current level
                this.getConditions().addAll(node.getConditions());
            }
        }
        return this;
    }


    /**
     * 返回一个用于遍历 SQL 树节点的迭代器.
     *
     * @return 迭代器
     * @since 1.0.0
     */
    @Override
    public Iterator<Tree> iterator() {
        return new Itr(this);
    }

    /**
     * SQL 树的迭代器实现.
     * @since 1.0.0
     */
    static class Itr implements Iterator<Tree> {

        private Tree current;

        /**
         * 构造一个从指定根节点开始的迭代器.
         *
         * @param root 树的根节点
         * @since 1.0.0
         */
        public Itr(Tree root) {
            current = root;
        }

        /**
         * {@inheritDoc}
         * @since 1.0.0
         */
        @Override
        public boolean hasNext() {
            return current != null;
        }

        /**
         * {@inheritDoc}
         * @since 1.0.0
         */
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