package io.github.luminion.sqlbooster.model.query;

import io.github.luminion.sqlbooster.enums.SqlKeyword;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.*;

/**
 * 条件组
 *
 * @author luminion
 * @since 1.0.0
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ConditionNode implements Iterable<ConditionNode> {
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
    protected ConditionNode next;

    @Override
    public Iterator<ConditionNode> iterator() {
        return new Itr(this);
    }


    /**
     * 添加一组条件
     * @param conditions 条件
     * @param connector 条件间的连接符号
     * @return this
     */
    public ConditionNode appendConditions(Collection<Condition> conditions, String connector) {
        if (conditions == null || conditions.isEmpty()) {
            return this;
        }
        connector = SqlKeyword.replaceConnector(connector);
        if (SqlKeyword.OR.getKeyword().equals(connector)) {
            // append or conditions to last node
            ConditionNode node = new ConditionNode();
            node.conditions.addAll(conditions);
            node.connector = connector;
            ConditionNode current = this;
            while (current.getNext() != null) {
                current = current.getNext();
            }
            current.next = node;
        } else {
            // put and conditions to current level
            this.getConditions().addAll(conditions);
        }
        return this;
    }

    /**
     * 合并另一节点的条件
     *
     * @param segment 条件
     * @return this
     */
    public ConditionNode merge(ConditionNode segment) {
        if (segment == null) {
            return this;
        }
        for (ConditionNode node : segment) {
            if (node.getConditions().isEmpty()) {
                continue;
            }
            this.appendConditions(node.getConditions(), node.getConnector());
        }
        return this;
    }

    static class Itr implements Iterator<ConditionNode> {

        private ConditionNode current;

        public Itr(ConditionNode root) {
            current = root;
        }

        @Override
        public boolean hasNext() {
            return current != null;
        }

        @Override
        public ConditionNode next() {
            if (current == null) {
                throw new NoSuchElementException();
            }
            ConditionNode result = current;
            current = current.getNext();
            return result;
        }
    }

}