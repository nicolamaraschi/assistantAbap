import React, { useState } from 'react';
import styled from 'styled-components';
import { FiSearch, FiX } from 'react-icons/fi';

const SearchBar = ({ onSearch, placeholder = 'Cerca...' }) => {
  const [searchTerm, setSearchTerm] = useState('');
  
  const handleChange = (e) => {
    const value = e.target.value;
    setSearchTerm(value);
    if (onSearch) {
      onSearch(value);
    }
  };
  
  const handleClear = () => {
    setSearchTerm('');
    if (onSearch) {
      onSearch('');
    }
  };
  
  return (
    <SearchContainer>
      <SearchIcon>
        <FiSearch />
      </SearchIcon>
      <SearchInput
        type="text"
        value={searchTerm}
        onChange={handleChange}
        placeholder={placeholder}
      />
      {searchTerm && (
        <ClearButton onClick={handleClear}>
          <FiX />
        </ClearButton>
      )}
    </SearchContainer>
  );
};

const SearchContainer = styled.div`
  position: relative;
  width: 100%;
  margin-bottom: 15px;
`;

const SearchIcon = styled.div`
  position: absolute;
  left: 10px;
  top: 50%;
  transform: translateY(-50%);
  color: #999;
  display: flex;
  align-items: center;
`;

const SearchInput = styled.input`
  width: 100%;
  padding: 10px 35px;
  border: 1px solid #ddd;
  border-radius: 4px;
  font-size: 14px;
  transition: border-color 0.3s, box-shadow 0.3s;
  
  &:focus {
    outline: none;
    border-color: #0066cc;
    box-shadow: 0 0 0 3px rgba(0, 102, 204, 0.2);
  }
`;

const ClearButton = styled.button`
  position: absolute;
  right: 10px;
  top: 50%;
  transform: translateY(-50%);
  background: transparent;
  border: none;
  color: #999;
  cursor: pointer;
  display: flex;
  align-items: center;
  padding: 0;
  
  &:hover {
    color: #333;
  }
`;

export default SearchBar;