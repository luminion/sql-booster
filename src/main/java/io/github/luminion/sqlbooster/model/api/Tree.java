package io.github.luminion.sqlbooster.model.api;

import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * 条件树接口.
 * <p>
 * 定义了用于构建复杂查询的树形结构. 每个节点可以包含一组条件和逻辑连接符, 并可以拥有一个子树.
 *
 * @author luminion
 * @since 1.0.0
 */
public interface Tree extends Iterable<Tree> {

    /**
     * 获取当前节点的条件列表.
     *
     * @return 条件列表
     * @since 1.0.0
     */
    Collection<Condition> getConditions();

    /**
     * 获取用于连接当前节点条件的逻辑连接符 (例如 AND, OR).
     *
     * @return 连接符
     * @since 1.0.0
     */
    String getConnector();

    /**
     * 获取子条件树.
     *
     * @return 子条件树, 可能为 null
     * @since 1.0.0
     */
    Tree getChild();

    /**
     * 返回一个用于遍历 SQL 树节点的迭代器.
     *
     * @return 迭代器
     * @since 1.0.0
     */
    @Override
    @SuppressWarnings("all")
    default Iterator<Tree> iterator() {
        return new Itr(this);
    }

    /**
     * SQL 树的迭代器实现.
     * @since 1.0.0
     */
    class Itr implements Iterator<Tree> {

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